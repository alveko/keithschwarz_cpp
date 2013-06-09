/****************************************************************************
 * File: VanEmdeBoasTree.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a set of integers backed by a van Emde Boas tree. A van
 * Emde Boas tree (or vEB-tree) has excellent performance guarantees - each
 * insertion, deletion, and lookup takes O(lg lg n) time, which is much better
 * than a standard BST or skiplist.  Moreover, it's possible to look up a
 * number's successor or predecessor (the smallest element larger than the
 * value, or the largest element smaller than the value, respectively) in time
 * O(lg lg n).  This makes vEB-trees extremely attractive when working with
 * large sets of integers.
 *
 * The main drawback of a vEB-tree is its large memory usage.  To store a 
 * collection of numbers in the range {0, 1, 2, ..., n - 1} takes O(n) memory.
 * For this reason, vEB-trees are not used very commonly.  When they are, they
 * usually use memory-saving optimization involving perfect hash tables.
 *
 * The internal structure of a vEB-tree is rather clever.  Suppose that you are
 * interested in storing some subset of the values in {0, 1, 2, ..., n - 1} in
 * the tree.  Let s = ceil(lg n); then 2^s is the smallest power of two at
 * least as large as n.  For now, assume that s is even; we'll deal with the
 * case where it isn't later.  This means that each of the values {0, 1, ...,
 * n - 1} can be written using a total of s bits.  Each value can therefore be
 * thought of as the concatenation of two s/2-bit numbers, which we'll refer to
 * as (high, low).  The vEB-tree works by maintaining an array of pointers to
 * s/2 smaller vEB-trees.  Whenever an element is looked up, its first s/2 bits
 * are used as the index into the smaller trees, which are then searched for
 * the remaining s/2 bits.  Since at each step of this process the lookup
 * halves the number of unvisited bits, the search will stop after lg s steps.
 * Since s = O(lg n), this lookup requires a total of O(lg lg n) steps, as
 * required.
 *
 * The main challenge of the vEB-tree is how to get successor and predecessor
 * working efficiently.  This requires a fairly clever set of tricks.  First,
 * at each level of the tree, we'll have the vEB-tree store the smallest and
 * largest elements of the tree specially.  These values won't be stored in the
 * tree proper, but rather as auxiliary data.  Next, in addition to the s/2
 * pointers to vEB-trees of size s/2, there will be one extra vEB-tree of size
 * s/2 containing a "summary" of the smaller trees.  This summary tree wil hold
 * values corresponding to which subtrees are nonempty.  For example, if the
 * vEB-tree holds values in its zeroth, fourth, and ninth subtrees, then this
 * summary tree would hold zero, four, and nine.
 *
 * To look up the successor of the element x, we begin by looking as the max
 * value of the subtree where x would normally be located.  If x is less than
 * this value, then we recursively search that tree for the successor, since
 * one must exist.  However, if x is at least as large as the max element (or
 * no max element is defined because the tree is empty), then the successor of
 * x must be the smallest element in the next nonempty subtree of the vEB-tree.
 * To find this tree, we do a successor query in the summary tree to find the
 * first nonempty tree, then return the minimum value of that tree.  A search
 * for x's predecessor works similarly.
 *
 * The memory usage of a vEB-tree is a bit tricky to analyze.  The memory used
 * by a vEB-tree for values up to n is given by
 *
 *    M(n) <= k0 sqrt(n) + (sqrt(n) + 1) M(sqrt(n)) 
 *    M(1)  = k1
 *
 * For some appropriate constants k0, k1.  We'll show that M(n) = O(n) by 
 * showing that M(n) <= max{c, rn + t} for some appropriate constants c, r, 
 * and t.  
 *
 * As a base case, for n = 1, we get that
 *
 *     M(1) = k1 <= c
 *
 * So if we pick c = k1, this holds.
 *
 * For the inductive case, we consider two cases - one in which sqrt(n) = 1
 * (where sqrt(n) is actually floor(sqrt(n))), and one in which sqrt(n) > 1.
 * This first case holds when n < 4:
 *
 *     M(n) <= k0 sqrt(n) + k1(sqrt(n) + 1)
 *           = (k0 + k1) sqrt(n) + 1
 *           = k0 + k1 + 1
 *
 * So we need to ensure that rn + t >= k0 + k1 + 1 when n = 2, 3.  Let's worry
 * about this later.  For the case where n >= 4:
 *
 *     M(n) <= k0 sqrt(n) + (sqrt(n) + 1)(r sqrt(n) + t) 
 *           = k0 sqrt(n) + r n + t sqrt(n) + r sqrt(n) + t
 *           = r n + (k0 + t + r) sqrt(n) + t
 *          <= r n + (k0 + t + r) sqrt(n) + t
 *
 * If we pick t such that r + t + k0 <= 0, then this expression is bounded by
 * rn + t and we're done.
 *
 * We now need to pick values of r and t such that
 *
 *    r + t + k0 <= 0
 *    2r + t >= k0 + k1 + 1
 *
 * Equivalently
 *
 *    -r - t >= k0
 *    2t + t >= k0 + k1 + 1
 *
 * Adding gives
 *
 *     r >= 2k0 + k1 + 1
 *
 * Substituting this in gives us
 *
 *     4k0 + 2k1 + 2 + t >= k0 + k1 + 1
 *     t >= -3k0 - k1 - 1
 *
 * and
 *
 *     2k0 + k1 + 1 + t + k0 < 0
 *     3k0 + k1 + 1 + t < 0
 *     t <= -3k0 - k1 - 1
 *
 * This means that if we pick
 *
 *     r =  2k0 + k1 + 1
 *     t = -3k0 - k1 - 1
 *     c =        k1
 *
 * Then the recurrence holds, and the memory usage is O(n).
 *
 * As a simplification, this implementation presents a VanEmdeBoasTree that
 * stores unsigned shorts, which are 16-bit values.  Each node stores an array
 * pointers to the next level down, as well as a pointer to the summary tree.
 * The min and max are stored, along with the number of elements in each tree.
 * This value allows us to quickly check whether the tree is empty, but does
 * not obviate the need for a summary structure.
 */
#ifndef VanEmdeBoasTree_Included
#define VanEmdeBoasTree_Included

#include <utility>  // For pair
#include <iterator> // For iterator, bidirectional_iterator_tag, reverse_iterator
#include <climits>  // For CHAR_BIT, ULONG_MAX

/**
 * A class representing a vEB-tree of unsigned shorts.
 */
class VanEmdeBoasTree {
public:
  /**
   * Constructor: VanEmdeBoasTree();
   * Usage: VanEmdeBoasTree myTree;
   * --------------------------------------------------------------------------
   * Constructs a new, empty vEB-tree.
   */
  VanEmdeBoasTree();

  /* Destructor: ~VanEmdeBoasTree();
   * Usage: (implicit)
   * --------------------------------------------------------------------------
   * Deallocates all memory allocated by the vEB-tree.
   */
  ~VanEmdeBoasTree();

  /**
   * Copy functions: VanEmdeBoasTree(const VanEmdeBoasTree& other);
   *                 VanEmdeBoasTree& operator= (const VanEmdeBoasTree& other);
   * Usage: VanEmdeBoasTree one = two;
   *        one = two;
   * --------------------------------------------------------------------------
   * Sets this VanEmdeBoasTree to be a deep-copy of some other vEB-tree.
   */
  VanEmdeBoasTree(const VanEmdeBoasTree& other);
  VanEmdeBoasTree& operator= (const VanEmdeBoasTree& other);

  /**
   * bool empty() const;
   * Usage: if (tree.empty()) { ... }
   * --------------------------------------------------------------------------
   * Returns whether this vEB contains no elements.
   */
  bool empty() const;

  /**
   * size_t size() const;
   * Usage: while (tree.size() > 1) { ... }
   * --------------------------------------------------------------------------
   * Returns the number of elements stored in the vEB-tree.
   */
  size_t size() const;

  /**
   * Type: const_iterator
   * --------------------------------------------------------------------------
   * A type representing an object that can visit but not modify the elements
   * of the vEB tree in sorted order.
   */
  class const_iterator;

  /**
   * const_iterator begin() const;
   * const_iterator end() const;
   * Usage: for (VanEmdeBoasTree::const_iterator itr = tree.begin();
   *             itr != tree.end(); ++itr) { ... }
   * --------------------------------------------------------------------------
   * Returns a range of iterators delineating the full contents of this
   * VanEmdeBoasTree.
   */
  const_iterator begin() const;
  const_iterator end() const;

  /**
   * Type: const_reverse_iterator
   * --------------------------------------------------------------------------
   * A type representing an object that can visit but not modify the elements
   * of the vEB tree in reverse sorted order.
   */
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  /**
   * const_reverse_iterator rbegin() const;
   * const_reverse_iterator rend() const;
   * Usage: for (VanEmdeBoasTree::const_reverse_iterator itr = tree.rbegin();
   *             itr != tree.rend(); ++itr) { ... }
   * --------------------------------------------------------------------------
   * Returns a range of iterators delineating the full contents of this
   * VanEmdeBoasTree in reverse order.
   */
  const_reverse_iterator rbegin() const;
  const_reverse_iterator rend() const;

  /**
   * const_iterator find(unsigned short value) const;
   * Usage: if (tree.find(137) != tree.end()) { ... }
   * --------------------------------------------------------------------------
   * Returns an iterator to the element in the tree with the specified value,
   * or end() as a sentinel if one does not exist.
   */
  const_iterator find(unsigned short value) const;

  /**
   * const_iterator predecessor(unsigned short value) const;
   * const_iterator successor(unsigned short value) const;
   * Usage: VanEmdeBoasTree::const_iterator itr = tree.predecessor(137);
   *        if (itr != end()) cout << *itr << endl;
   * --------------------------------------------------------------------------
   * predecessor returns an iterator to the first element in the tree whose
   * key is strictly less than the specified value (or end() if one does not
   * exist).  successor returns an iterator to the first element in the tree
   * whose key is strictly greater than the specified value (or end() if one
   * does not exist).
   */
  const_iterator predecessor(unsigned short value) const;
  const_iterator successor(unsigned short value) const;

  /**
   * std::pair<const_iterator, bool> insert(unsigned short value);
   * Usage: tree.insert(137);
   * --------------------------------------------------------------------------
   * Inserts the specified value into the vEB tree.  If the value did not
   * exist in the tree prior to the call, the return value is true paired with
   * an iterator to the element.  Otherwise, the return value is false paired
   * with an iterator to the value.
   */
  std::pair<const_iterator, bool> insert(unsigned short value);

  /**
   * bool erase(unsigned short value);
   * bool erase(const_iterator where);
   * Usage: tree.erase(137);  tree.erase(tree.begin());
   * --------------------------------------------------------------------------
   * Removes the element with the specified key (or the element indicated by
   * the specified const_iterator) from the vEB tree, returning whether the
   * element existed and was removed (true) or not.
   */
  bool erase(unsigned short value);
  bool erase(const_iterator where);

  /**
   * void swap(VanEmdeBoasTree& rhs);
   * Usage: tree.swap(otherTree);
   * --------------------------------------------------------------------------
   * Exchanges the contents of this vEB-tree and some other vEB-tree in O(1)
   * time and space.
   */
  void swap(VanEmdeBoasTree& rhs);

private: 
  /* A type representing a vEB-tree structure.  It stores the min and max
   * elements at the current level of the tree, an array of pointers to smaller
   * vEB trees, and a pointer to a summary vEB tree.
   *
   * This vEB-tree implementation uses two major optimizations.  First, rather
   * than storing the complete tree structure, once the number of bits in
   * consideration becomes sufficiently small (say, such that the values are
   * only four bits long), we bottom out and use a bit array instead of the
   * standard implementation.  This saves an enormous amount of overhead, since
   * a bit array is substantially more compact than all of the necessary
   * pointers to sublevels.
   *
   * Second, because each vEB-tree node stores a fixed-sized array whose length
   * varies from level to level, we design the structure intending to store the
   * pointers to subtrees beyond the end of the struct by overallocating space
   * for it.  This is a standard optimization that avoids a lot of unnecessary
   * pointer indirections.
   *
   * This struct's layout is very brittle.  The position of the the first array 
   * element must be the very last member, since the overallocated memory needs
   * to be flush against the array itself.
   */
  struct Node {
    /* The min and max values here. */
    unsigned short mMin, mMax;

    /* Whether either of these values are set. */
    bool mIsEmpty;

    /* A pointer to the summary structure.  This is typed as a void* because
     * at a certain point, this pointer will not point at a Node, but rather
     * at a block of raw memory acting as a bitvector.
     */
    void* mSummary;

    /* An array of one element, representing the first of (possibly) many
     * pointers to subtrees.  This MUST be the last element of the struct!
     * We use a void* here because this might actually be pointing at a bit
     * array, rather than another Node.
     */
    void* mChildren[1];
    
    /* Operator new overallocates the node to ensure space exists for the Node
     * pointers.  The argument to this function is the number of extra pointers
     * that should be allocated.
     */
    void* operator new (size_t size, size_t numPointers);

    /* Operator delete only needed in case the constructor throws.  There is
     * no constructor, so this is technically not needed, but in the interests
     * of forward-thinking we provide it anyway.
     */
    void operator delete (void* memory, size_t numBits);
  };

  /* A pointer to the root vEB-tree node. */
  void* mRoot;

  /* A cache of the size of the tree. */
  size_t mSize;

  /* Internally, this class uses size_t's to represent "either a valid unsigned
   * short or a sentinel indicating that nothing exists."  Thiss constant 
   * represents the "not actually a value" term.
   */
  static const size_t kNil = ULONG_MAX;

  /* Make const_iterator a friend so it can access internal structure. */
  friend class const_iterator;

  /* Helper function to recursively construct a vEB-tree to hold the specified
   * number of bits.  Because this might just return a bit array, the function
   * returns a void*.
   */
  static void* recCreateTree(size_t numBits);

  /* Helper function to recursively clone a vEB-tree holding the specified
   * number of bits.
   */
  static void* recCloneTree(void* root, size_t numBits);

  /* Helper function to recursively destroy a vEB-tree of the specified number
   * of bits.
   */
  static void recDeleteTree(void* root, size_t numBits);

  /* Helper function to recursively search the tree for a value, reporting
   * whether or not it exists.
   */
  static bool recFindElement(unsigned short value, void* root, size_t numBits);

  /* Helper function to recursively insert an entry into the tree, reporting
   * whether the value was added (true) or already existed (false).
   */
  static bool recInsertElement(unsigned short value, void* root, size_t numBits);

  /* Helper function to recursively delete an entry from the tree, reporting
   * whether it already existed.
   */
  static bool recEraseElement(unsigned short value, void* root, size_t numBits);

  /* Helper function to return the largest or smallest elements of a vEB-tree,
   * handing back the sentinel if the tree is empty.
   */
  static size_t treeMax(void* root, size_t numBits);
  static size_t treeMin(void* root, size_t numBits);

  /* Helper function to return whether a tree is empty. */
  static bool isTreeEmpty(void* root, size_t numBits);

  /* Helper function to return the successor or predecessor of a given entry in
   * the tree, or the sentinel if none exists.
   */
  static size_t recSuccessor(unsigned short value, void* root, size_t numBits);
  static size_t recPredecessor(unsigned short value, void* root, size_t numBits);
};

/* Definition of the const_iterator type. */
class VanEmdeBoasTree::const_iterator:
  public std::iterator<std::bidirectional_iterator_tag, const unsigned short> {
public:
  /* Default constructor creates a garbage const_iterator. */
  const_iterator();

  /* Forwards and backwards motion. */
  const_iterator& operator++ ();
  const_iterator& operator-- ();
  const const_iterator operator++ (int);
  const const_iterator operator-- (int);
  
  /* Pointer dereference.  No arrow is defined because the iterator visits
   * unsigned shorts.  Since the values are immutable, we hand back a value
   * rather than a reference.
   */
  const unsigned short operator* () const;

  /* Equality and disequality testing. */
  bool operator== (const const_iterator& rhs) const;
  bool operator!= (const const_iterator& rhs) const;

private:
  /* Make VanEmdeBoasTree a friend of this class so it can invoke the private
   * constructor.
   */
  friend class VanEmdeBoasTree;

  /* Constructor creates an iterator that starts off at the specified value. */
  const_iterator(size_t valueOrSentinel, const VanEmdeBoasTree* owner);

  /* Internally, the iterator works by maintaining a size_t containing either
   * the value currently being iterated over, or kNil as a sentinel.  The ++
   * and -- operators just call predecessor and successor on the owner tree to
   * move to the previous and next values.
   */
  size_t mCurr;
  const VanEmdeBoasTree* mOwner;
};

#endif
