/******************************************************************************
 * File: TernarySearchTree.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a sorted associative array backed by a ternary search
 * tree.  I cannot conclusively determine who first developed the ternary
 * search tree.  The first major discussion of ternary search trees seems to be
 * by Robert Sedgewick and Jon Bentley in their article "Fast Algorithms for
 * Sorting and Searching Strings," though an almost identical structure was
 * described by Sleator and Tarjan in their landmark paper "Self-Adjusting
 * Binary Search Trees," which also introduced the splay tree.  Both articles
 * mention that the data structure has its roots as early as the 1960s.  But
 * independently of who first invented the structure, it's a quite impressive
 * idea.
 *
 * The ternary search tree is a hybridization of a binary search tree and a
 * trie that is designed to store strings.  In a binary search tree, each node
 * would store a string, with each child pointing, respectively, to the words
 * lexicographically lower than and lexicographically greater than the string
 * held at the current node.  In a balanced tree at most O(lg n) comparisons
 * are needed to find a particular word, but each comparison takes time O(k),
 * where k is the number of characters in the string in question.  This gives
 * a runtime of O(k lg n).  In a trie, a word can be looked up in time O(k) by
 * simply indexing into the child array at each level, proceeding to the node
 * in question.  However, the trie uses an exorbitant amount of memory; each
 * node has size O(|S|), where S is the alphabet from which the strings are
 * drawn.  This means that a trie holding n different strings could in theory
 * use O(n |S|) memory, whereas the binary search tree would use memory
 * proportional only to the total number of characters in each of the strings.
 *
 * The ternary search tree aims to get the benefits of both structures as
 * follows.  Each ternary search tree is implemented as a layer of binary
 * search trees, where each node stores a single character along with a pointer
 * to another ternary search tree (TST).  The intution behind this structure is
 * as follows.  To search for a word in the TST, begin by doing a standard BST
 * search for the first letter of the word in the BST at the top level of the
 * TST.  If the letter is not found, the word is not in the TST and we are
 * done.  Otherwise, there is some node in the TST holding the first letter of
 * the word in question.  Recursively descend into the sub-TST pointed at by
 * this node.  In this way, you can think of a TST as a hierarchical BST, where
 * each node as a left, right, and "middle" pointer; the left and right pointer
 * corresponding to the left and right pointers in a regular BST, and the
 * middle pointer storing all values that compare equal to the given value.
 *
 * Assuming that we use some sort of balancing algorithm on each of the BSTs
 * the TST is composed of, the time required to search is TST is quite fast.
 * Each BST in the TST holds at most |S| nodes, one for each character in the
 * alphabet, and we can search it in O(lg |S|) time.  If we're looking for a
 * string of length k, this means that the lookup time is O(k lg |S|), which is
 * asymptotically better than the O(k lg n) time required in the BST search and
 * only marginally worse than the O(k) time required for the trie search.
 * The memory usage of a TST is quite good compared to a trie: the trie uses
 * space O(n |S|) in the worst case, while the memory usage of a TST is O(nk),
 * where k is the average number of characters in a string stored in the TST
 * (since each node holds one character).  The memory usage for a BST of
 * strings is also O(nk), since we have to store each character in each string,
 * meaning that, at least asymptotically, the two have comparable memory usage.
 * This means that a TST is asymptotically no worse than a BST in memory usage,
 * and in the case of sparse data sets (where the average number of characters
 * in each string is, say, o(|S|)) is asymptotically better than the trie.
 * 
 * This implementation of the ternary search tree uses the TST to implement a
 * sorted associative container of strings, much in the same way that an STL
 * set<string> would behave.  It is parameterized over the type of string being
 * stored, and uses template specialization to allow TSTs of arbitrary
 * instantiations of std::basic_string.
 */
#ifndef TernarySearchTree_Included
#define TernarySearchTree_Included

#include <string> // For basic_string, char_traits
#include <map>
#include <deque>
#include <iterator>
#include <algorithm>

/* An implementation of a set of strings using a ternary search tree. */
template <typename Ch, typename Traits = std::char_traits<Ch> > 
class TernarySearchTree {
public:
  /**
   * Constructor: TernarySearchTree();
   * Usage: TernarySearchTree<char> myTree;
   * --------------------------------------------------------------------------
   * Constructs a new, empty ternary search tree.
   */
  TernarySearchTree();

  /**
   * Destructor: ~TernarySearchTree();
   * Usage: (implicit)
   * --------------------------------------------------------------------------
   * Destroys the ternary search tree, deallocating all memory allocated 
   * internally.
   */
  ~TernarySearchTree();

  /**
   * Copy functions: TernarySearchTree(const TernarySearchTree& other);
   *                 TernarySearchTree& operator= (const TernarySearchTree& other);
   * Usage: TernarySearchTree<string, int> one = two;
   *        one = two;
   * --------------------------------------------------------------------------
   * Makes this ternary search tree equal to a deep-copy of some other ternary search tree.
   */
  TernarySearchTree(const TernarySearchTree& other);
  TernarySearchTree& operator= (const TernarySearchTree& other);

  /**
   * Type: const_iterator
   * --------------------------------------------------------------------------
   * A type representing an iterator that can traverse the ternary search tree,
   * visiting each string in ascending order.
   */
  class const_iterator;

  /**
   * Type: value_type
   * --------------------------------------------------------------------------
   * The type of the strings stored in this ternary search tree.
   */
  typedef std::basic_string<Ch, Traits> value_type;

  /**
   * std::pair<const_iterator, bool> insert(const value_type& str);
   * Usage: myTST.insert("Skiplist");
   * -------------------------------------------------------------------------
   * Inserts the specified string into the ternary search tree, returning a
   * pair of an iterator to the string in the ternary search tree and a boolean
   * value indicating whether the string was inserted (true) or already existed
   * (false).
   */
  std::pair<const_iterator, bool> insert(const value_type& str);

  /**
   * bool erase(const value_type& key);
   * Usage: myTST.erase("AVL Tree");
   * -------------------------------------------------------------------------
   * Removes the entry from the ternary search tree with the specified key, if
   * it exists.  Returns whether or not an element was erased.  All 
   * outstanding iterators remain valid, except for those referencing the 
   * deleted element.
   */
  bool erase(const value_type& key);

  /**
   * const_iterator erase(const_iterator where);
   * Usage: myTST.erase(myTST.begin());
   * -------------------------------------------------------------------------
   * Removes the entry referenced by the specified iterator from the tree,
   * returning an iterator to the next element in the sequence.
   */
  const_iterator erase(const_iterator where);

  /**
   * const_iterator find(const value_type& str);
   * Usage: if (myTST.find("Skiplist") != myTST.end()) { ... }
   * -------------------------------------------------------------------------
   * Returns an iterator to the indicated entry in the ternary search tree, or
   * end() as as sentinel if it does not exist.
   */
  const_iterator find(const value_type& str) const;

  /**
   * const_iterator begin() const;
   * const_iterator end() const;
   * Usage: for (TernarySearchTree<char>::iterator itr = t.begin();
   *             itr != t.end(); ++itr) { ... }
   * -------------------------------------------------------------------------
   * Returns iterators delineating the full contents of the ternary search 
   * tree.
   */
  const_iterator begin() const;
  const_iterator end() const;

  /**
   * const_iterator lower_bound(const value_type& str) const;
   * const_iterator upper_bound(const value_type& str) const;
   * Usage: for (TernarySearchTree<char>::iterator itr = t.lower_bound("AVL");
   *             itr != t.upper_bound("skiplist"); ++itr) { ... }
   * -------------------------------------------------------------------------
   * lower_bound returns an iterator to the first element in the ternary search
   * tree whose value is at least as large as str.  upper_bound returns an 
   * iterator to the first element in the ternary search tree whose value is 
   * strictly greater than str.
   */
  const_iterator lower_bound(const value_type& str) const;
  const_iterator upper_bound(const value_type& str) const;

  /**
   * std::pair<const_iterator, const_iterator> 
   *    equal_range(const value_type& str) (const);
   * Usage: std::pair<TernarySearchTree<char>::iterator, 
   *                  TernarySearchTree<char>::iterator>
   *          range = t.equal_range("AVL");
   * -------------------------------------------------------------------------
   * Returns a range of iterators spanning the unique copy of the entry whose
   * value is str if it exists, and otherwise a pair of iterators both pointing
   * to the spot in the ternary search tree where the element would be if it 
   * were.
   */
  std::pair<const_iterator, const_iterator> equal_range(const value_type& key) const;

  /**
   * size_t size() const;
   * Usage: cout << "TernarySearchTree contains " << s.size() << " entries." << endl;
   * -------------------------------------------------------------------------
   * Returns the number of elements stored in the ternary search tree.
   */
  size_t size() const;

  /**
   * bool empty() const;
   * Usage: if (s.empty()) { ... }
   * -------------------------------------------------------------------------
   * Returns whether the ternary search tree contains no elements.
   */
  bool empty() const;

  /**
   * void swap(TernarySearchTree& other);
   * Usage: one.swap(two);
   * -------------------------------------------------------------------------
   * Exchanges the contents of this ternary search tree and some other ternary
   * search tree.  All outstanding iterators are invalidated.
   */
  void swap(TernarySearchTree& other);

private:
  /* Comparator: CharComparator
   * --------------------------------------------------------------------------
   * A comparison struct that uses the less-than operation provided by the
   * traits class to compare two characters.
   */
  struct CharComparator {
    bool operator() (const Ch& one, const Ch& two) const {
      return std::char_traits<Ch>::lt(one, two);
    }
  };

  /* A type representing some node in the ternary search tree.  It is intended
   * that this structure will be stored inside of a some class (such as
   * std::map) that implements a binary search tree, such that it will be
   * augmented with left and right pointers.
   */
  struct Node;

  /* Type: CharTST
   * --------------------------------------------------------------------------
   * A type representing a ternary search tree.  This is represented as a map
   * from characters to nodes, where the map provides the BST structure (with
   * balancing) and the node provides the pointer to the nested TST.
   */
  typedef std::map<Ch, Node*, CharComparator> CharTST;

  /* A type representing a node in the ternary search tree.  Each node stores a
   * boolean flag indicating whether there is some word that ends here, along
   * with a pointer to a nested ternary search tree.
   */
  struct Node {
    const Ch    mLetter; // The character encoded by this node.  If the parent
                         // is NULL, this value is unspecified.
    Node* const mParent; // A pointer to the TST node which contains this node
                         // as an element of its child tree.
    bool        mIsWord; // Whether the sequence so far is a valid word.
    CharTST     mEqual;  // The TST of strings starting with this character.
    
    /* Constructor: Node(Ch ch, Node* parent, bool isWord);
     * Usage: new Node(true, parent);
     * ------------------------------------------------------------------------
     * Constructs a new Node from the specified information
     */
    Node(Ch ch, Node* parent, bool isWord) 
      : mLetter(ch), mParent(parent), mIsWord(isWord) {
      // Handled in initializer list
    }
  };

  /* A pointer to the root node of the TST. */
  Node* mRoot;

  /* The number of strings stored here, cached for efficiency. */
  size_t mSize;

  /* Make const_iterator a friend so that it can access the internal structure
   * of this class.
   */
  friend class const_iterator;

  /* Utility function to recursively delete a TST. */
  static void deleteTree(Node* tree);

  /* Utility function to recursively copy a TST.  The first parameter encodes
   * what tree to clone; the second is what node the copied tree should use
   * as its parent.
   */
  static Node* cloneTree(Node* tree, Node* parent);

  /* Utility function which, given a node, finds the lexicographically first
   * word in the TST rooted at that node.
   */
  static Node* firstWordIn(Node* tree);
};

/* Comparison operators */
template <typename Ch, typename Traits>
bool operator<  (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);
template <typename Ch, typename Traits>
bool operator== (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);
template <typename Ch, typename Traits>
bool operator<= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);
template <typename Ch, typename Traits>
bool operator!= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);
template <typename Ch, typename Traits>
bool operator>= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);
template <typename Ch, typename Traits>
bool operator>  (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs);

/* * * * * Implementation Below This Point * * * * */

/* Definition of const_iterator type. This iterator is, essentially, a depth-
 * first search over the ternary search tree.  Each iterator keeps track of a 
 * stack of all the nodes in the chain from the root to the current node, then
 * uses std::map operations to keep on advancing.
 */

template <typename Ch, typename Traits>
class TernarySearchTree<Ch, Traits>::const_iterator:
  public std::iterator<std::forward_iterator_tag, value_type> {
public:
  /* Constructs a new const_iterator with an unspecified value. */
  const_iterator();

  /* Returns whether two iterators are equal or unequal. */
  bool operator== (const const_iterator& rhs) const;
  bool operator!= (const const_iterator& rhs) const;

  /* Dereferences the const_iterator and returns the stored string. */
  const value_type& operator*  () const;
  const value_type* operator-> () const;

  /* Advances the iterator one step. */
  const_iterator& operator++ ();
  const const_iterator operator++ (int);

private:
  /* Constructs a new const_iterator that traverses the indicated TST starting
   * from the given node.  If the node is NULL, a sentinel iterator is created.
   */
  explicit const_iterator(const TernarySearchTree* tst, Node* where);

  /* A reference to the TST that created this const_iterator. */
  const TernarySearchTree* mTST;

  /* A stack of the nodes corresponding to the letters in the string. */
  std::deque<Node*> mTrace;

  /* A string corresponding to the characters we've accumulated together so
   * far.  We store this externally from the DFS stack so we don't have to sca\n
   * the stacks over and over again on each dereference.
   */
  value_type mString;

  /* Make the ternary search tree a friend so that it can access the private
   * constructor.
   */
  friend class TernarySearchTree;
};

/* Default const_iterator constructor sets the stored TST to NULL and doesn't
 * fill in any of the stack frames.
 */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>::const_iterator::const_iterator()
  : mTST(NULL) {
  // Handled in initializer list.
}

/* Parameterized constructor stores the indicated trace, assuming that it's
 * been populated correctly.
 */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>::const_iterator::const_iterator(const TernarySearchTree* tst,
                                                              Node* where) 
  : mTST(tst) {
  /* Walk back up from this node to the root node, assembling the sequence of
   * nodes that got us here.
   */
  for (Node* curr = where; curr != NULL; curr = curr->mParent)
    mTrace.push_front(curr);

  /* Given this trace, assemble the current string by concatenating the symbols
   * of everything except the first node, which is the root.
   */
  for (size_t i = 1; i < mTrace.size(); ++i)
    mString += mTrace[i]->mLetter;
}

/* Equality checks whether the underlying TST and trace are equivalent. */
template <typename Ch, typename Traits>
bool TernarySearchTree<Ch, Traits>::const_iterator::operator== (const const_iterator& rhs) const {
  return mTST == rhs.mTST && mTrace == rhs.mTrace;
}

/* Disequality implemented in terms of equality. */
template <typename Ch, typename Traits>
bool TernarySearchTree<Ch, Traits>::const_iterator::operator!= (const const_iterator& rhs) const {
  return !(*this == rhs);
}

/* Pointer operators work by returning the stored pointer.  This is safe only
 * because we're handing back const references and pointers.
 */
template <typename Ch, typename Traits>
const typename TernarySearchTree<Ch, Traits>::value_type&
TernarySearchTree<Ch, Traits>::const_iterator::operator*  () const {
  return mString;
}
template <typename Ch, typename Traits>
const typename TernarySearchTree<Ch, Traits>::value_type*
TernarySearchTree<Ch, Traits>::const_iterator::operator-> () const {
  return &**this;
}

/* Pointer advance operator works by continuing on from the last location if
 * possible, backing up and continuing earlier on if that can't be done.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator&
TernarySearchTree<Ch, Traits>::const_iterator::operator++ () {
  /* When the search left off, we hit some node that was indicated to be the
   * end of a word.  There are two cases to consider.  First, if this is a leaf
   * node, then we need to advance to the next node in the current BST.  If,
   * however, we can't do this (for instance, if this is the last node in the
   * current BST), then we need to walk up the DFS stack until we find the
   * first iterator where this is possible.  These two cases are tricky to
   * unify.  One represents a condition represents us setting up the iteration
   * over the children, and one represents moving forward in that iteration.
   * To unify them, this function is split into two parts.  The first part
   * works by moving the DFS search to the first node that hasn't been visited
   * yet, and the second by then descending as far as possible into that tree.
   */
  
  /* If the last node visited has children, keep exploring downward until we
   * find the first word end that we can.  This line is dense, but it means
   * "does the iterator in the topmost frame correspond to a node with a child
   * tree?"
   */
  if (!mTrace.back()->mEqual.empty()) {
    /* See what the first thing in this subtree is. */
    typename CharTST::const_iterator itr = mTrace.back()->mEqual.begin();

    /* Put this node onto the explore stack. */
    mTrace.push_back(itr->second);

    /* Append this character to the current string. */
    mString.push_back(itr->first);
  }
  /* Otherwise, if the last node has no children, then we're at some leaf in
   * the tree and need to advance forward.
   */
  else {
    /* Try walking the iterator from the topmost stack frame forward.  If we
     * can't, unwind the stack until we find a spot where we can move.
     */
    while (true) {
      /* Cache what the last character of the string is; we'll need this so we
       * can look up what comes next.
       */
      Ch lastChar = mTrace.back()->mLetter;

      /* Back up one level in the search.  If this empties our stack, it means
       * that we've explored everything and are done.
       */
      mTrace.pop_back();
      if (mTrace.empty())
        return *this;

      /* Get rid of the last character from the string; we've just gotten rid
       * of that level.
       */
      mString.erase(mString.end() - 1);

      /* Get an iterator to the next child of the current node by using the map
       * upper_bound operation.
       */
      typename CharTST::const_iterator itr = mTrace.back()->mEqual.upper_bound(lastChar);

      /* If this iterator is valid, then that's where we'll continue the search
       * from.
       */
      if (itr != mTrace.back()->mEqual.end()) {
        /* Set our new location to this spot. */
        mTrace.push_back(itr->second);

        /* Append to the current string the character we just found. */
        mString.push_back(itr->first);
        break;
      }
      
      /* Otherwise, we need to walk up the stack a bit more, so we'll continue
       * the search again.
       */
    }
  }
    
  /* At this point, we're looking at some node which we have previously not
   * explored.  Continuously move downward until we hit a node that is marked
   * as a word ending.
   */
  while (!mTrace.back()->mIsWord) {
    /* Get an iterator to the first child of this node. */
    typename CharTST::const_iterator itr = mTrace.back()->mEqual.begin();

    /* Update the stack by exploring its child. */
    mTrace.push_back(itr->second);

    /* Update the accumulated string. */
    mString.push_back(itr->first);
  }

  /* Finally, as per contract, hand back a reference to this object. */
  return *this;
}

/* Postfix ++ implemented in terms of prefix ++. */
template <typename Ch, typename Traits>
const typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::const_iterator::operator++ (int) {
  const_iterator result = *this; // Cache our value,
  ++*this;                       // then advance to the next spot,
  return result;                 // then hand back the old value.
}

/***** Implementation of TernarySearchTree *****/

/* Constructor sets the size to be zero and leaves the tree empty. */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>::TernarySearchTree() : mRoot(NULL), mSize(0) {
  // Handled in initializer list
}

/* Destructor recursively walks the tree structure, freeing everything it 
 * finds.
 */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>::~TernarySearchTree() {
  deleteTree(mRoot);
}

/* Recursively deleting a tree entails a postorder traversal that deletes the
 * nodes after they're expanded.
 */
template <typename Ch, typename Traits>
void TernarySearchTree<Ch, Traits>::deleteTree(Node* root) {
  /* Deleting an empty tree is trivial; we just don't do anything. */
  if (!root) return;

  /* Scan across all the key/value pairs, recursively deleting each nested
   * node.
   */
  for (typename CharTST::const_iterator itr = root->mEqual.begin(); 
       itr != root->mEqual.end(); ++itr)
    deleteTree(itr->second);

  /* Finally, delete the node itself. */
  delete root;
}

/* Inserting a value walks down the TST structure adding nodes as appropriate.
 * It also sets the "is word" flag on the final node.
 */
template <typename Ch, typename Traits>
std::pair<typename TernarySearchTree<Ch, Traits>::const_iterator, bool>
TernarySearchTree<Ch, Traits>::insert(const value_type& str) {
  /* This function suffers from an "off-by-one" bug because the letters in the
   * string are encoded as key/value pairs in the Node maps.  Consequently,
   * before we even start looking at the letters of the string, we'll confirm
   * that the root node exists.
   */
  if (!mRoot)
    mRoot = new Node(Ch(), NULL, false);

  /* This function could be written recursively, but I've opted to write it
   * iteratively for the challenge.  At each stage we'll maintain a pointer to
   * the current node we're on in the search.
   */
  Node* curr = mRoot;

  /* For each character in the string, walk down the TST, adding nodes where
   * appropriate.
   */
  for (size_t i = 0; i < str.size(); ++i) {
    /* Look up the node associated with the current character of the string.
     * If it doesn't exist, create one.
     */
    Node* next = curr->mEqual[str[i]];

    /* If this node is NULL, go create a new node for this spot.  Then pretend
     * that this node was here all along.
     */
    if (!next)
      next = curr->mEqual[str[i]] = new Node(str[i], curr, false);

    /* Move into this node. */
    curr = next;
  }

  /* At this point we're looking at the correct node for this entry.  If the
   * word already existed, then return an indicator to that effect.
   */
  if (curr->mIsWord)
    return std::make_pair(const_iterator(this, curr), false);

  /* Otherwise, mark that the word is here. */
  curr->mIsWord = true;

  /* Increase the size - we just added something - and hand back an iterator
   * to the current location.
   */
  ++mSize;
  return std::make_pair(const_iterator(this, curr), true);
}

/* find searches for the node by doing a standard walk down the tree.  If at
 * any point we walk off the tree, or if we arrive at the proper node and no
 * word is there, we signal failure.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::find(const value_type& str) const {
  /* Begin by checking if the root is NULL.  If so, then there aren't any
   * words here and we're done.
   */
  if (!mRoot) return end();

  /* Walk down the tree to our destination. */
  Node* curr = mRoot;
  for (size_t i = 0; i < str.size(); ++i) {
    /* Look up the location of the next node, failing if there is no entry
     * here.
     */
    typename CharTST::const_iterator next = curr->mEqual.find(str[i]);
    if (next == curr->mEqual.end())
      return end();

    /* Continue searching from there. */
    curr = next->second;
  }

  /* Hand back an iterator to this node if we found what we were looking for
   * and end() as a sentinel otherwise.
   */
  return curr->mIsWord? const_iterator(this, curr) : end();
}



/* begin hands back an iterator to the first complete word in the tree. */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::begin() const {
  return const_iterator(this, firstWordIn(mRoot));
}

/* To get the first word in a particular tree, we just walk down the tree from
 * the root downward, always taking the smallest-labeled root.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::Node*
TernarySearchTree<Ch, Traits>::firstWordIn(Node* root) {
  /* If the tree is empty, there are no words here. */
  if (root == NULL) return NULL;

  /* Descend all the way down the tree until we find something that's a word.
   * Our descent works by proceding to the first element of the subtree, which
   * is given by a pretty cryptic set of accesses (first to the current node,
   * then to value of the subtree's first entry).  Note that this won't walk
   * off the end of the tree because all leaf nodes are words.
   */
  while(!root->mIsWord)
    root = root->mEqual.begin()->second;
  
  return root;
}

/* end just hands back a sentinel iterator. */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::end() const {
  return const_iterator(this, NULL);
}

/* size just hands back the stored size. */
template <typename Ch, typename Traits>
size_t TernarySearchTree<Ch, Traits>::size() const {
  return mSize;
}

/* empty hands back whether the size is zero. */
template <typename Ch, typename Traits>
bool TernarySearchTree<Ch, Traits>::empty() const {
  return size() == 0;
}

/* Non-iterator version of erase implemented in terms of iterator version of
 * erase.
 */
template <typename Ch, typename Traits>
bool TernarySearchTree<Ch, Traits>::erase(const value_type& str) {
  /* Grab an iterator to the value to erase. */
  const_iterator toErase = find(str);

  /* If we didn't find anything, we're done. */
  if (toErase == end()) return false;

  /* Otherwise, actually erase it, then hand back success. */
  erase(toErase);
  return true;
}

/* Erasing a value from the ternary search tree works by walking from the value
 * to erase upwards, cleaning up nodes that no longer need to be there.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::erase(const_iterator where) {
  /* Begin by getting the current node out of the iterator.  This is stored at
   * the top of its stack of nodes.
   */
  Node* curr = where.mTrace.back();

  /* Advance this iterator forward one step so that we have the value to
   * return.
   */
  ++where;

  /* Drop the number of elements here; we just lost a word. */
  --mSize;

  /* Mark that this node no longer corresponds to a word. */
  curr->mIsWord = false;

  /* Now, we need to start cleaning up nodes from the tree that are no longer
   * necessary.  In particular, if this node has no children, then we'll want
   * to delete this node and all of the nodes above it until we hit some node
   * that corresponds to a word.
   */

  /* This loop is a bit tricky.  We want to walk all the way back up to the
   * root of the tree, deleting nodes that need to be cleaned up.  To do so,
   * we'll keep walking upward while the following invariants hold:
   *
   * 1. The current node is not NULL.
   * 2. The current node has no children.
   * 3. The current node is not a word.
   *
   * If these three conditions hold, we can just get rid of the entry.
   */
  while (curr && curr->mEqual.empty() && !curr->mIsWord) {
    /* Remove this node from its parent.  This takes on two cases.  First, if
     * the current node is the root node (i.e. it has no parent), then we set
     * the root of the tree to NULL since there are no more nodes.  Second, if
     * the current is not the root node, then we need to remove the node from
     * its parent's BST.
     */
    if (!curr->mParent)
      mRoot = NULL;
    else
      curr->mParent->mEqual.erase(curr->mLetter);
 
    /* Clean up this node, then set it to its parent. */
    Node* next = curr->mParent;
    delete curr;
    curr = next;
  }

  /* Hand back the iterator we produced earlier. */
  return where;
}

/* Copy constructor recursively clones the tree. */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>::TernarySearchTree(const TernarySearchTree& rhs) {
  /* Copy over the size field. */
  mSize = rhs.mSize;

  /* Clone the underlying tree structure. */
  mRoot = cloneTree(rhs.mRoot, NULL);
}

/* Cloning a tree involves recursively walking the tree's fields and deep-
 * copying them.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::Node*
TernarySearchTree<Ch, Traits>::cloneTree(Node* root, Node* parent) {
  /* The empty tree is a copy of itself. */
  if (root == NULL) return NULL;

  /* Duplicate the main node. */
  Node* result = new Node(root->mLetter, parent, root->mIsWord);

  /* Set the new node's children to deep copies of the original node's
   * children.
   */
  for (typename CharTST::const_iterator itr = root->mEqual.begin();
       itr != root->mEqual.end(); ++itr)
    result->mEqual[itr->first] = cloneTree(itr->second, result);

  return result;
}

/* Assignment operator implemented using copy-and-swap. */
template <typename Ch, typename Traits>
TernarySearchTree<Ch, Traits>&
TernarySearchTree<Ch, Traits>::operator= (const TernarySearchTree& rhs) {
  TernarySearchTree copy = rhs;
  swap(copy);
  return *this;
}

/* swap just exchanges the size and root pointers. */
template <typename Ch, typename Traits>
void TernarySearchTree<Ch, Traits>::swap(TernarySearchTree& other) {
  std::swap(mSize, other.mSize);
  std::swap(mRoot, other.mRoot);
}

/* The lower_bound function is interesting.  It works by continuously walking
 * down the tree as usual while the characters match.  If we either walk off
 * the tree or don't find the character in question, then we search for the
 * word in the tree that's the smallest successor of the given node.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::lower_bound(const value_type& str) const {
  /* First, if the tree is empty, just hand back end() as a sentinel. */
  if (mRoot == NULL) return end();

  /* Second, if the query is the empty string, hand back begin() as the first
   * string no lower than it.
   */
  if (str.empty()) return begin();

  /* Now, start walking down the tree according to the usual rules. */
  Node* curr = mRoot;
  for (size_t i = 0; i < str.size(); ++i) {
    /* Check whether this character exists. */
    typename CharTST::const_iterator next = curr->mEqual.find(str[i]);

    /* There are now two cases to consider.  First, if we found the letter in
     * question, we can just walk down into that part of the tree.
     */
    if (next != curr->mEqual.end())
      curr = next->second;

    /* Otherwise, we need to find the successor word.  We do this by walking
     * back up to the root trying to find some BST node that comes after the
     * current node.
     */
    else {
      /* Scan backwards over the characters, trying to find something with a
       * nontrivial successor.
       */
      for (int backtrack = i; backtrack >= 0; --backtrack) {
        /* Use the STL upper_bound algorithm to find the first character that
         * is a successor of the expected character.
         */
        typename CharTST::const_iterator successor = curr->mEqual.upper_bound(str[backtrack]);
        
        /* If we found the successor, descend into it and scan down to the
         * first word we find there.
         */
        if (successor != curr->mEqual.end())
          return const_iterator(this, firstWordIn(successor->second));

        /* Otherwise back up a level. */
        curr = curr->mParent;
      }

      /* If we're here, then we walked all the way up to the root without
       * finding anything.  Since we know the input isn't the empty string,
       * we know that any word here can't be the best one, and so we can just
       * give up.
       */
      return end();
    }
  }

  /* If we walked all the way down this path successfully, then the lower
   * bound is the first string in the subtree rooted at where the search
   * stopped.
   */
  return const_iterator(this, firstWordIn(curr));
}

/* upper_bound works by looking at the result of lower_bound and increasing it
 * if it happens to be equal to the query string.
 */
template <typename Ch, typename Traits>
typename TernarySearchTree<Ch, Traits>::const_iterator
TernarySearchTree<Ch, Traits>::upper_bound(const value_type& str) const {
  /* Use lower_bound to get an iterator that's not less than the input. */
  const_iterator result = lower_bound(str);

  /* If we got an end iterator, then it's also the upper bound. */
  if (result == end()) return result;

  /* Otherwise, if the resulting word equals the input word, advance the
   * iterator forward a step.
   */
  if (*result == str)
    ++result;
  
  return result;
}

/* equal_range just pairs together lower_bound and upper_bound. */
template <typename Ch, typename Traits>
std::pair<typename TernarySearchTree<Ch, Traits>::const_iterator,
          typename TernarySearchTree<Ch, Traits>::const_iterator>
TernarySearchTree<Ch, Traits>::equal_range(const value_type& str) const {
  return std::make_pair(lower_bound(str), upper_bound(str));
}

/* Comparison operators == and < use the standard STL algorithms. */
template <typename Ch, typename Traits>
bool operator<  (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(),
                                      rhs.begin(), rhs.end());
}
template <typename Ch, typename Traits>
bool operator== (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), 
                                                rhs.begin());
}

/* Remaining comparisons implemented in terms of the above comparisons. */
template <typename Ch, typename Traits>
bool operator<= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  /* x <= y   iff !(x > y)   iff !(y < x) */
  return !(rhs < lhs);
}
template <typename Ch, typename Traits>
bool operator!= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  return !(lhs == rhs);
}
template <typename Ch, typename Traits>
bool operator>= (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  /* x >= y   iff !(x < y) */
  return !(lhs < rhs);
}
template <typename Ch, typename Traits>

bool operator>  (const TernarySearchTree<Ch, Traits>& lhs,
                 const TernarySearchTree<Ch, Traits>& rhs) {
  /* x > y iff y < x */
  return rhs < lhs;
}

#endif
