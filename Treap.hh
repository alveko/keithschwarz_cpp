/****************************************************************************
 * File: Treap.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a dictionary structure backed by a treap.  A treap is
 * a probabilistic data structure in which each node stores two values, the
 * key being stored and a random 32-bit integer.  The tree structure is such
 * that the tree is simultaneously a binary search tree with respect to the
 * keys and a min-heap with respect to the integers.  The BST property
 * guarantees O(h) insertion, deletion, and lookup (where h is the height of
 * the tree), and the min-heap property helps ensure that, on expectation, the
 * height of the tree is not too great.  In fact, with high probability the
 * height of the tree is O(lg n), and the treap has the same runtime
 * guarantees as a self-balancing binary tree.
 *
 * The main advantage of the treap over other balanced tree structures is
 * that the implementation is substantially similar.  All operations to do
 * insertions and deletions can be implemented as simple tree rotations.  For
 * example, inserting a value into a treap works by inserting the node as
 * usual in a BST, then using tree rotations to fix up the min-heap property.
 * Deletions work by rotating the node to be deleted down to a leaf, then
 * removing it from the tree.  Neither operation requires complex logic for
 * balance factors, node colors, etc.
 *
 * This implementation of the Treap uses the Treap to implement a sorted
 * associative array, where each node stores both a key and some auxiliary
 * data.  Additionally, each node functions as a member of a doubly-linked
 * list of the elements, which allows for linear time tree traversal even when
 * the treap is being modified.  Fortunately, since the list contains elements
 * in sorted order, the links do not need to be modified during a rotation.
 */
#ifndef Treap_Included
#define Treap_Included

#include <algorithm>   // For lexicographical_compare, equal, max
#include <functional>  // For less
#include <utility>     // For pair
#include <iterator>    // For iterator, reverse_iterator
#include <cstdlib>     // For rand
#include <stdexcept>   // For out_of_range

/**
 * A map-like class backed by a treap.
 */
template <typename Key, typename Value, typename Comparator = std::less<Key> >
class Treap {
public:
  /**
   * Constructor: Treap(Comparator comp = Comparator());
   * Usage: Treap<string, int> myTreap;
   * Usage: Treap<string, int> myTreap(MyComparisonFunction);
   * -------------------------------------------------------------------------
   * Constructs a new, empty treap that uses the indicated comparator to
   * compare keys.
   */
  Treap(Comparator comp = Comparator());

  /**
   * Destructor: ~Treap();
   * Usage: (implicit)
   * -------------------------------------------------------------------------
   * Destroys the treap, deallocating all memory allocated internally.
   */
  ~Treap();

  /**
   * Copy functions: Treap(const Treap& other);
   *                 Treap& operator= (const Treap& other);
   * Usage: Treap<string, int> one = two;
   *        one = two;
   * -------------------------------------------------------------------------
   * Makes this treap equal to a deep-copy of some other treap.
   */
  Treap(const Treap& other);
  Treap& operator= (const Treap& other);

  /**
   * Type: iterator
   * Type: const_iterator
   * -------------------------------------------------------------------------
   * A pair of types that can traverse the elements of a treap in ascending
   * order.
   */
  class iterator;
  class const_iterator;

  /**
   * Type: reverse_iterator
   * Type: const_reverse_iterator
   * -------------------------------------------------------------------------
   * A pair of types that can traverse the elements of a treap in descending
   * order.
   */
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  /**
   * std::pair<iterator, bool> insert(const Key& key, const Value& value);
   * Usage: myTreap.insert("Skiplist", 137);
   * -------------------------------------------------------------------------
   * Inserts the specified key/value pair into the treap.  If an entry with
   * the specified key already existed, this function returns false paired
   * with an iterator to the extant value.  If the entry was inserted
   * successfully, returns true paired with an iterator to the new element.
   */
  std::pair<iterator, bool> insert(const Key& key, const Value& value);

  /**
   * bool erase(const Key& key);
   * Usage: myTreap.erase("AVL Tree");
   * -------------------------------------------------------------------------
   * Removes the entry from the treap with the specified key, if it exists.
   * Returns whether or not an element was erased.  All outstanding iterators
   * remain valid, except for those referencing the deleted element.
   */
  bool erase(const Key& key);

  /**
   * iterator erase(iterator where);
   * Usage: myTreap.erase(myTreap.begin());
   * -------------------------------------------------------------------------
   * Removes the entry referenced by the specified iterator from the tree,
   * returning an iterator to the next element in the sequence.
   */
  iterator erase(iterator where);

  /**
   * iterator find(const Key& key);
   * const_iterator find(const Key& key);
   * Usage: if (myTreap.find("Skiplist") != myTreap.end()) { ... }
   * -------------------------------------------------------------------------
   * Returns an iterator to the entry in the treap with the specified key, or
   *  end() as as sentinel if it does not exist.
   */
  iterator find(const Key& key);
  const_iterator find(const Key& key) const;

  /**
   * Value& operator[] (const Key& key);
   * Usage: myTreap["skiplist"] = 137;
   * -------------------------------------------------------------------------
   * Returns a reference to the value associated with the specified key in the
   * treap.  If the key is not contained in the treap, it will be inserted
   * into the treap with a default-constructed Entry as its value.
   */
  Value& operator[] (const Key& key);

  /**
   * Value& at(const Key& key);
   * const Value& at(const Key& key) const;
   * Usage: myTreap.at("skiplist") = 137;
   * -------------------------------------------------------------------------
   * Returns a reference to the value associated with the specified key,
   * throwing a std::out_of_range exception if the key does not exist in the
   * treap.
   */
  Value& at(const Key& key);
  const Value& at(const Key& key) const;

  /**
   * (const_)iterator begin() (const);
   * (const_)iterator end() (const);
   * Usage: for (Treap<string, int>::iterator itr = t.begin(); 
   *             itr != t.end(); ++itr) { ... }
   * -------------------------------------------------------------------------
   * Returns iterators delineating the full contents of the treap.  Each
   * iterator acts as a pointer to a std::pair<const Key, Entry>.
   */
  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /**
   * (const_)reverse_iterator rbegin() (const);
   * (const_)reverse_iterator rend() (const);
   * Usage: for (Treap<string, int>::reverse_iterator itr = s.rbegin(); 
   *             itr != s.rend(); ++itr) { ... }
   * -------------------------------------------------------------------------
   * Returns iterators delineating the full contents of the treap in reverse
   * order.
   */
  reverse_iterator rbegin();
  reverse_iterator rend();
  const_reverse_iterator rbegin() const;
  const_reverse_iterator rend() const;

  /**
   * (const_)iterator lower_bound(const Key& key) (const);
   * (const_)iterator upper_bound(const Key& key) (const);
   * Usage: for (Treap<string, int>::iterator itr = t.lower_bound("AVL");
   *             itr != t.upper_bound("skiplist"); ++itr) { ... }
   * -------------------------------------------------------------------------
   * lower_bound returns an iterator to the first element in the treap whose
   * key is at least as large as key.  upper_bound returns an iterator to the
   * first element in the treap whose key is strictly greater than key.
   */
  iterator lower_bound(const Key& key);
  iterator upper_bound(const Key& key);
  const_iterator lower_bound(const Key& key) const;
  const_iterator upper_bound(const Key& key) const;

  /**
   * std::pair<(const_)iterator, (const_)iterator> 
   *    equal_range(const Key& key) (const);
   * Usage: std::pair<Treap<int, int>::iterator, Treap<int, int>::iterator>
   *          range = t.equal_range("AVL");
   * -------------------------------------------------------------------------
   * Returns a range of iterators spanning the unique copy of the entry whose
   * key is key if it exists, and otherwise a pair of iterators both pointing
   * to the spot in the treap where the element would be if it were.
   */
  std::pair<iterator, iterator> equal_range(const Key& key);
  std::pair<const_iterator, const_iterator> equal_range(const Key& key) const;

  /**
   * size_t size() const;
   * Usage: cout << "Treap contains " << s.size() << " entries." << endl;
   * -------------------------------------------------------------------------
   * Returns the number of elements stored in the treap.
   */
  size_t size() const;

  /**
   * bool empty() const;
   * Usage: if (s.empty()) { ... }
   * -------------------------------------------------------------------------
   * Returns whether the treap contains no elements.
   */
  bool empty() const;

  /**
   * void swap(Treap& other);
   * Usage: one.swap(two);
   * -------------------------------------------------------------------------
   * Exchanges the contents of this treap and some other treap.  All
   * outstanding iterators are invalidated.
   */
  void swap(Treap& other);

private:
  /* A type representing a node in the treap. */
  struct Node {
    std::pair<const Key, Value> mValue; // The actual value stored here
    const int mPriority;                // The priority of this node

    /* The children are stored in an array to make it easier to implement tree
     * rotations.  The first entry is the left child, the second the right.
     */
    Node* mChildren[2];

    /* Pointer to the parent node. */
    Node* mParent;

    /* Pointer to the next and previous node in the sorted sequence. */
    Node* mNext, *mPrev;

    /* Constructor sets up the value to the specified key/value pair, and
     * sets up the node's priority.
     */
    Node(const Key& key, const Value& value, int priority);
  };

  /* A pointer to the first and last elements of the treap. */
  Node* mHead, *mTail;

  /* A pointer to the root of the tree. */
  Node* mRoot;

  /* The comparator to use when storing elements. */
  Comparator mComp;

  /* The number of elements in the list. */
  size_t mSize;

  /* A utility base class for iterator and const_iterator which actually
   * supplies all of the logic necessary for the two to work together.  The
   * parameters are the derived type, the type of a pointer being visited, and
   * the type of a reference being visited.  This uses the Curiously-Recurring
   * Template Pattern to work correctly.
   */
  template <typename DerivedType, typename Pointer, typename Reference>
  class IteratorBase;
  template <typename DerivedType, typename Pointer, typename Reference>
  friend class IteratorBase;

  /* Make iterator and const_iterator friends as well so they can use the
   * Node type.
   */
  friend class iterator;
  friend class const_iterator;

  /* A utility function to perform a tree rotation to pull the child above its
   * parent.
   */
  void rotateUp(Node* child);

  /* A utility function which does a BST search on the tree to look up where
   * a given key resides, returning a pointer to the node if it's found and
   * NULL otherwise.  This function is marked const but returns a non-const
   * pointer so that it can be used in both versions of the find() function.
   * It is assumed that the rest of the implementation will ensure that const-
   * ness is not violated.
   */
  Node* findNode(const Key& key) const;

  /* A utility function which, given a node and the node to use as its parent,
   * recursively deep-copies the tree rooted at that node, using the parent
   * node as the new tree's parent.
   */
  static Node* cloneTree(Node* toClone, Node* parent);

  /* A utility function which, given a tree and a pointer to the predecessor
   * of that tree, rewires the linked list in that tree to represent an
   * inorder traversal.  No fields are modified.  The return value is the node
   * with the highest key.
   */
  static Node* rethreadLinkedList(Node* root, Node* predecessor);

  /* A utility function which, given a key, looks up where in the tree that
   * key would be were it contained.  As with findNode, the constness of the
   * return type does not match the constness of the function and it is
   * expected that the implementation will enforce constness itself.
   */
  Node* findNodePosition(const Key& key) const;
};

/* Comparison operators for Treaps. */
template <typename Key, typename Value, typename Comparator>
bool operator<  (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator<= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator== (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator!= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator>= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator>  (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs);

/* * * * * Implementation Below This Point * * * * */

/* Definition of the IteratorBase type, which is used to provide a common
 * implementation for iterator and const_iterator.
 */
template <typename Key, typename Value, typename Comparator>
template <typename DerivedType, typename Pointer, typename Reference>
class Treap<Key, Value, Comparator>::IteratorBase {
public:
  /* Utility typedef to talk about nodes. */
  typedef typename Treap<Key, Value, Comparator>::Node Node;

  /* Advance operators just construct derived type instances of the proper
   * type, then advance them.
   */
  DerivedType& operator++ () {
    mCurr = mCurr->mNext;

    /* Downcast to our actual type. */
    return static_cast<DerivedType&>(*this);
  }
  const DerivedType operator++ (int) {
    /* Copy our current value by downcasting to our real type. */
    DerivedType result = static_cast<DerivedType&>(*this);

    /* Advance to the next element. */
    ++*this;

    /* Hand back the cached value. */
    return result;
  }

  /* Backup operators work on the same principle. */
  DerivedType& operator-- () {
    /* If the current pointer is NULL, it means that we've walked off the end
     * of the structure and need to back up a step.
     */
    if (mCurr == NULL) {
      mCurr = mOwner->mTail;
    }
    /* Otherwise, just back up a step. */
    else {
      mCurr = mCurr->mPrev;
    }

    /* Downcast to our actual type. */
    return static_cast<DerivedType&>(*this);
  }
  const DerivedType operator-- (int) {
    /* Copy our current value by downcasting to our real type. */
    DerivedType result = static_cast<DerivedType&>(*this);

    /* Back up a step. */
    --*this;

    /* Hand back the cached value. */
    return result;
  }

  /* Equality and disequality operators are parameterized - we'll allow anyone
   * whose type is IteratorBase to compare with us.  This means that we can
   * compare both iterator and const_iterator against one another.
   */
  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator== (const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) {
    /* Just check the underlying pointers, which (fortunately!) are of the 
     * same type.
     */
    return mOwner == rhs.mOwner && mCurr == rhs.mCurr;
  }
  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator!= (const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) {
    /* We are disequal if equality returns false. */
    return !(*this == rhs);
  }

  /* Pointer dereference operator hands back a reference. */
  Reference operator* () const {
    return mCurr->mValue;
  }
  
  /* Arrow operator returns a pointer. */
  Pointer operator-> () const {
    /* Use the standard "&**this" trick to dereference this object and return
     * a pointer to the referenced value.
     */
    return &**this;
  }

protected:
  /* Which Treap we belong to.  This pointer is const even though we are
   * possibly allowing ourselves to modify the treap elements to avoid having
   * to duplicate this logic once again for const vs. non-const iterators.
   */
  const Treap* mOwner;

  /* Where we are in the list. */
  Node* mCurr;

  /* In order for equality comparisons to work correctly, all IteratorBases
   * must be friends of one another.
   */
  template <typename Derived2, typename Pointer2, typename Reference2>
  friend class IteratorBase;

  /* Constructor sets up the treap and node pointers appropriately. */
  IteratorBase(const Treap* owner = NULL, Node* curr = NULL) 
  : mOwner(owner), mCurr(curr) {
    // Handled in initializer list
  }
};

/* iterator and const_iterator implementations work by deriving off of
 * IteratorBase, passing in parameters that make all the operators work.
 * Additionally, we inherit from std::iterator to import all the necessary
 * typedefs to qualify as an iterator.
 */
template <typename Key, typename Value, typename Comparator>
class Treap<Key, Value, Comparator>::iterator:
  public std::iterator< std::bidirectional_iterator_tag,
                        std::pair<const Key, Value> >,
  public IteratorBase<iterator,                       // Our type
                      std::pair<const Key, Value>*,   // Reference type
                      std::pair<const Key, Value>&> { // Pointer type 
public:
  /* Default constructor forwards NULL to base implicity. */
  iterator() {
    // Nothing to do here.
  }

  /* All major operations inherited from the base type. */

private:
  /* Constructor for creating an iterator out of a raw node just forwards this
   * argument to the base type.  This line is absolutely awful because the
   * type of the base is so complex.
   */
  iterator(const Treap* owner,
           typename Treap<Key, Value, Comparator>::Node* node) :
    IteratorBase<iterator,
                 std::pair<const Key, Value>*,
                 std::pair<const Key, Value>&>(owner, node) {
    // Handled by initializer list
  }

  /* Make the Treap a friend so it can call this constructor. */
  friend class Treap;

  /* Make const_iterator a friend so we can do iterator-to-const_iterator
   * conversions.
   */
  friend class const_iterator;
};

/* Same as above, but with const added in. */
template <typename Key, typename Value, typename Comparator>
class Treap<Key, Value, Comparator>::const_iterator:
  public std::iterator< std::bidirectional_iterator_tag,
                        const std::pair<const Key, Value> >,
  public IteratorBase<const_iterator,                       // Our type
                      const std::pair<const Key, Value>*,   // Reference type
                      const std::pair<const Key, Value>&> { // Pointer type 
public:
  /* Default constructor forwards NULL to base implicity. */
  const_iterator() {
    // Nothing to do here.
  }

  /* iterator conversion constructor forwards the other iterator's base fields
   * to the base class.
   */
  const_iterator(iterator itr) :
    IteratorBase<const_iterator,
                 const std::pair<const Key, Value>*,
                 const std::pair<const Key, Value>&>(itr.mOwner, itr.mCurr) {
    // Handled in initializer list
  }

  /* All major operations inherited from the base type. */

private:
  /* See iterator implementation for details about what this does. */
  const_iterator(const Treap* owner,
                 typename Treap<Key, Value, Comparator>::Node* node) :
    IteratorBase<const_iterator,
                 const std::pair<const Key, Value>*,
                 const std::pair<const Key, Value>&>(owner, node) {
    // Handled by initializer list
  }
  
  /* Make the Treap a friend so it can call this constructor. */
  friend class Treap;
};

/**** Treap::Node Implementation. ****/

/* Constructor sets up the value and priority, but leaves everything else
 * unset.  This is mostly to allow the fields to be const while still getting
 * the code to compile.
 */
template <typename Key, typename Value, typename Comparator>
Treap<Key, Value, Comparator>::Node::Node(const Key& key,
                                          const Value& value,
                                          int priority) : mValue(key, value),
                                                          mPriority(priority) {
  // Handled in initializer list.
}

/**** Treap Implementation ****/

/* Constructor sets up a new, empty Treap. */
template <typename Key, typename Value, typename Comparator>
Treap<Key, Value, Comparator>::Treap(Comparator comp) : mComp(comp) {
  /* Initially, the list of elements is empty and the tree is NULL. */
  mHead = mTail = mRoot = NULL;

  /* The tree is created empty. */
  mSize = 0;
}

/* Destructor walks the linked list of elements, deleting all nodes it
 * encounters.
 */
template <typename Key, typename Value, typename Comparator>
Treap<Key, Value, Comparator>::~Treap() {
  /* Start at the head of the list. */
  Node* curr = mHead;
  while (curr != NULL) {
    /* Cache the next value; we're about to blow up our only pointer to it. */
    Node* next = curr->mNext;

    /* Free memory, then go to the next node. */
    delete curr;
    curr = next;
  }
}

/* Inserting an element creates a new node with a random priority, does a BST
 * insert, then bubbles it up.
 */
template <typename Key, typename Value, typename Comparator>
std::pair<typename Treap<Key, Value, Comparator>::iterator, bool>
Treap<Key, Value, Comparator>::insert(const Key& key, const Value& value) {
  /* Recursively walk down the tree from the root, looking for where the value
   * should go.  In the course of doing so, we'll maintain some extra
   * information about the node's successor and predecessor so that we can
   * wire the new node in in O(1) time.
   *
   * The information that we'll need will be the last nodes at which we
   * visited the left and right child.  This is because if the new node ends
   * up as a left child, then its predecessor is the last ancestor on the path
   * where we followed its right pointer, and vice-versa if the node ends up
   * as a right child.
   */
  Node* lastLeft = NULL, *lastRight = NULL;
  
  /* Also keep track of our current location as a pointer to the pointer in
   * the tree where the node will end up, which allows us to insert the node
   * by simply rewiring this pointer.
   */
  Node** curr   = &mRoot;

  /* Also track the last visited node. */
  Node*  parent = NULL;

  /* Now, do a standard binary tree insert.  If we ever find the node, we can
   * stop early.
   */
  while (*curr != NULL) {
    /* Update the parent to be this node, since it's the last one visited. */
    parent = *curr;

    /* Check whether we belong in the left subtree. */
    if (mComp(key, (*curr)->mValue.first)) {
      lastLeft = *curr;
      curr = &(*curr)->mChildren[0];
    }
    /* ... or perhaps the right subtree. */
    else if (mComp((*curr)->mValue.first, key)) {
      lastRight = *curr; // Last visited node where we went right.
      curr = &(*curr)->mChildren[1];
    }
    /* Otherwise, the key must already exist in the tree, and we can just
     * return an iterator to it.
     */
    else
      return std::make_pair(iterator(this, *curr), false);
  }

  /* At this point we've found our insertion point and can create the node
   * we're going to wire in.  We'll assign it a random priority.
   */
  Node* toInsert = new Node(key, value, rand());
  
  /* Splice it into the tree. */
  toInsert->mParent = parent;
  *curr = toInsert;

  /* The new node has no children. */
  toInsert->mChildren[0] = toInsert->mChildren[1] = NULL;

  /* Wire this node into the linked list in-between its predecessor and
   * successor in the tree.  The successor is the last node where we went
   * left, and the predecessor is the last node where we went right.
   */
  toInsert->mNext = lastLeft;
  toInsert->mPrev = lastRight;

  /* Update the previous pointer of the next entry, or change the list tail
   * if there is no next entry.
   */
  if (toInsert->mNext)
    toInsert->mNext->mPrev = toInsert;
  else
    mTail = toInsert;

  /* Update the next pointer of the previous entry similarly. */
  if (toInsert->mPrev)
    toInsert->mPrev->mNext = toInsert;
  else
    mHead = toInsert;
  
  /* At this point, the node is in the right spot in the tree, and all that
   * remains is to reheapify with tree rotations.  We do this by continuously
   * rotating the tree while the node's parent's priority is greater than the
   * node's priority.
   */
  while (toInsert->mParent && toInsert->mParent->mPriority > toInsert->mPriority)
    rotateUp(toInsert);

  /* Increase the size of the tree, since we just added a node. */
  ++mSize;

  /* Hand back an iterator to the new element, along with a notification that
   * it was inserted correctly.
   */
  return std::make_pair(iterator(this, toInsert), true);
}

/* To perform a tree rotation, we identify whether we're doing a left or
 * right rotation, then rewrite pointers as follows:
 *
 * In a right rotation, we do the following:
 *
 *      B            A
 *     / \          / \
 *    A   2   -->  0   B
 *   / \              / \
 *  0   1            1   2
 *
 * In a left rotation, this runs backwards.
 *
 * The reason that we've implemented the nodes as an array of pointers rather
 * than using two named pointers is that the logic is symmetric.  If the node
 * is its left child, then its parent becomes its right child, and the node's
 * right child becomes the parent's left child.  If the node is its parent's
 * right child, then the node's parent becomes its left child and the node's
 * left child becomes the parent's right child.  In other words, the general
 * formula is
 *
 * If the node is its parent's SIDE child, then the parent becomes that node's
 * OPPOSITE-SIDE child, and the node's OPPOSITE-SIDE child becomes the
 * parent's SIDE child.
 *
 * This code also updates the root if the tree root gets rotated out.
 */
template <typename Key, typename Value, typename Comparator>
void Treap<Key, Value, Comparator>::rotateUp(Node* node) {
  /* Determine which side the node is on.  It's on the left (side 0) if the
   * parent's first pointer matches it, and is on the right (side 1) if the
   * node's first pointer doesn't match it.  This is, coincidentally, whether
   * the node is not equal to the first pointer of its root.
   */
  const int side = (node != node->mParent->mChildren[0]);
  
  /* The other side is the logical negation of the side itself. */
  const int otherSide = !side;

  /* Cache the displaced child and parent of the current node. */
  Node* child  = node->mChildren[otherSide];
  Node* parent = node->mParent;
  
  /* Shuffle pointers around to make the node the parent of its parent. */
  node->mParent = parent->mParent;
  node->mChildren[otherSide] = parent;

  /* Shuffle around pointers so that the parent takes on the displaced
   * child.
   */
  parent->mChildren[side] = child;
  if (child)
    child->mParent = parent;

  /* Update the grandparent (if any) so that its child is now the rotated
   * element rather than the parent.  If there is no grandparent, the node is
   * now the root.
   */
  if (parent->mParent) {
    const int parentSide = (parent != parent->mParent->mChildren[0]);
    parent->mParent->mChildren[parentSide] = node;
  } else
    mRoot = node;

  /* In either case, change the parent so that it now treats the node as the
   * parent.
   */
  parent->mParent = node;
}

/* Both versions of find work by calling findNode and wrapping the result up
 * in the appropriate iterator type.
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::find(const Key& key) {
  return iterator(this, findNode(key));
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_iterator
Treap<Key, Value, Comparator>::find(const Key& key) const {
  return const_iterator(this, findNode(key));
}

/* findNode just does a standard BST lookup. */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::Node*
Treap<Key, Value, Comparator>::findNode(const Key& key) const {
  /* Start the search at the root and work downwards. */
  Node* curr = mRoot;
  while (curr != NULL) {
    /* If the key is less than this node, go left. */
    if (mComp(key, curr->mValue.first))
      curr = curr->mChildren[0];
    /* Otherwise if the key is greater than the node, go right. */
    else if (mComp(curr->mValue.first, key))
      curr = curr->mChildren[1];
    /* Otherwise, we found the node. */
    else
      return curr;
  }

  /* If we got here, we fell off the tree without finding the node in
   * question and should return NULL to indicate this.
   */
  return NULL;
}

/* begin and end return iterators wrapping the head of the list or NULL,
 * respectively.
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::begin() {
  return iterator(this, mHead);
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_iterator
Treap<Key, Value, Comparator>::begin() const {
  return iterator(this, mHead);
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::end() {
  return iterator(this, NULL);
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_iterator
Treap<Key, Value, Comparator>::end() const {
  return iterator(this, NULL);
}

/* rbegin and rend return wrapped versions of end() and begin(),
 * respectively.
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::reverse_iterator
Treap<Key, Value, Comparator>::rbegin() {
  return reverse_iterator(end());
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_reverse_iterator
Treap<Key, Value, Comparator>::rbegin() const {
  return const_reverse_iterator(end());
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::reverse_iterator
Treap<Key, Value, Comparator>::rend() {
  return reverse_iterator(begin());
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_reverse_iterator
Treap<Key, Value, Comparator>::rend() const {
  return const_reverse_iterator(begin());
}

/* size just returns the cached size of the treap. */
template <typename Key, typename Value, typename Comparator>
size_t Treap<Key, Value, Comparator>::size() const {
  return mSize;
}

/* empty returns whether the size is zero. */
template <typename Key, typename Value, typename Comparator>
bool Treap<Key, Value, Comparator>::empty() const {
  return size() == 0;
}

/* To remove an element, we locate its position in the tree, rotate it down
 * until it becomes a leaf, and then remove it from the tree.
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::erase(iterator where) {
  /* Grab the node out of the iterator so we know where to start. */
  Node* node = where.mCurr;
  
  /* As long as the node has children, keep rotating the node's smaller child
   * upward.
   */
  while (node->mChildren[0] || node->mChildren[1]) {
    /* See which child to rotate upward.  There are four cases. */
    Node* toRotate;

    /* Case one: Only left child. */
    if (!node->mChildren[1]) 
      toRotate = node->mChildren[0];
    /* Case two: Only right child. */
    else if (!node->mChildren[0])
      toRotate = node->mChildren[1];
    /* Case 3: Both children, left has lower priority. */
    else if (node->mChildren[0]->mPriority < node->mChildren[1]->mPriority)
      toRotate = node->mChildren[0];
    /* Case 4: Both children, right has lower priority. */
    else
      toRotate = node->mChildren[1];

    /* Rotate it up! */
    rotateUp(toRotate);
  }

  /* Break the node out of the tree by cutting the connection to the
   * parent.
   */
  if (node->mParent) {
    /* Use our standard trick to clear the proper field. */
    node->mParent->mChildren[node->mParent->mChildren[0] != node] = NULL;
  }
  /* If there is no parent, the root was just cleared. */
  else
    mRoot = NULL;

  /* Next, we need to splice this node out of the list of entries. */

  /* If there is a next node, wire its previous pointer around the current
   * node.  Otherwise, the tail just changed.
   */
  if (node->mNext)
    node->mNext->mPrev = node->mPrev;
  else
    mTail = node->mPrev;

  /* If there is a previous node, wite its next pointer around the current
   * node.  Otherwise, the head just changed.
   */
  if (node->mPrev)
    node->mPrev->mNext = node->mNext;
  else
    mHead = node->mNext;

  /* Cache the next node; it's the value we're going to be returning. */
  Node* next = node->mNext;

  /* Delete the node and drop the number of nodes left, since we just got rid
   * of something.
   */
  delete node;
  --mSize;

  /* Finally, return an iterator to the next element. */
  return iterator(this, next);
}

/* Erasing a single value just calls find to locate the element and the
 * iterator version of erase to remove it.
 */
template <typename Key, typename Value, typename Comparator>
bool Treap<Key, Value, Comparator>::erase(const Key& key) {
  /* Look up where this node is, then remove it if it exists. */
  iterator where = find(key);
  if (where == end()) return false;

  erase(where);
  return true;
}

/* Square brackets implemented in terms of insert(). */
template <typename Key, typename Value, typename Comparator>
Value& Treap<Key, Value, Comparator>::operator[] (const Key& key) {
  /* Call insert to get a pair of an iterator and a bool.  Look at the
   * iterator, then consider its second field.
   */
  return insert(key, Value()).first->second;
}

/* at implemented in terms of find. */
template <typename Key, typename Value, typename Comparator>
const Value& Treap<Key, Value, Comparator>::at(const Key& key) const {
  /* Look up the key, failing if we can't find it. */
  const_iterator result = find(key);
  if (result == end())
    throw std::out_of_range("Key not found in treap.");

  /* Otherwise just return the value field. */
  return result->second;
}

/* non-const at implemented in terms of at using the const_cast/static_cast
 * trick.
 */
template <typename Key, typename Value, typename Comparator>
Value& Treap<Key, Value, Comparator>::at(const Key& key) {
  return const_cast<Value&>(static_cast<const Treap*>(this)->at(key));
}

/* The copy constructor is perhaps the most complex part of this entire
 * implementation.  It works in two passes.  First, the tree structure itself
 * is duplicated, without paying any attention to the next and previous
 * pointers threaded through.  Next, we run a recursive pass over the cloned
 * tree, fixing up all of the next and previous pointers as we go.
 */
template <typename Key, typename Value, typename Comparator>
Treap<Key, Value, Comparator>::Treap(const Treap& other) {
  /* Start off with the simple bits - copy over the size field and 
   * comparator. 
   */
  mSize = other.mSize;
  mComp = other.mComp;

  /* Clone the tree structure. */
  mRoot = cloneTree(other.mRoot, NULL);

  /* Rectify the linked list. */
  rethreadLinkedList(mRoot, NULL);

  /* Finally, fix up the first and last pointers of the list by looking for
   * the smallest and largest elements in the tree.
   */
  mTail = mHead = mRoot;
  while (mHead && mHead->mChildren[0]) mHead = mHead->mChildren[0];
  while (mTail && mTail->mChildren[1]) mTail = mTail->mChildren[1];
}

/* Cloning a tree is a simple structural recursion. */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::Node*
Treap<Key, Value, Comparator>::cloneTree(Node* toClone, Node* parent) {
  /* Base case: the clone of the empty tree is that tree itself. */
  if (toClone == NULL) return NULL;

  /* Create a copy of the node, moving over the priorities and key/value
   * pair.
   */
  Node* result = new Node(toClone->mValue.first, toClone->mValue.second,
                          toClone->mPriority);

  /* Recursively clone the subtrees. */
  for (int i = 0; i < 2; ++i)
    result->mChildren[i] = cloneTree(toClone->mChildren[i], result);

  /* Set the parent. */
  result->mParent = parent;

  return result;
}

/* Fixing up the doubly-linked list is a bit tricky.  The function acts as an
 * inorder traversal.  We first fix up the left subtree, getting a pointer to
 * the node holding the largest value in that subtree (the predecessor of this
 * node).  We then chain the current node into the linked list, then fix up
 * the nodes to the right (which have the current node as their predecessor).
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::Node*
Treap<Key, Value, Comparator>::rethreadLinkedList(Node* root, Node* predecessor) {
  /* Base case: if the root is null, then the largest element visited so far
   * is whatever we were told it was.
   */
  if (root == NULL) return predecessor;

  /* Otherwise, recursively fix up the left subtree using the actual
   * predecessor.  Store the return value as the new predecessor.
   */
  predecessor = rethreadLinkedList(root->mChildren[0], predecessor);

  /* Add ourselves to the linked list. */
  root->mPrev = predecessor;
  if (predecessor)
    predecessor->mNext = root;
  root->mNext = NULL;

  /* Recursively invoke on the right subtree, passing in this node as the
   * predecessor.
   */
  return rethreadLinkedList(root->mChildren[1], root);
}

/* Assignment operator implemented using copy-and-swap. */
template <typename Key, typename Value, typename Comparator>
Treap<Key, Value, Comparator>&
Treap<Key, Value, Comparator>::operator= (const Treap& other) {
  Treap clone = other;
  swap(clone);
  return *this;
}

/* swap just does an element-by-element swap. */
template <typename Key, typename Value, typename Comparator>
void Treap<Key, Value, Comparator>::swap(Treap& other) {
  /* Use std::swap to get the job done. */
  std::swap(mRoot, other.mRoot);
  std::swap(mSize, other.mSize);
  std::swap(mHead, other.mHead);
  std::swap(mTail, other.mTail);
  std::swap(mComp, other.mComp);
}

/* lower_bound just returns the proper position for the element in the tree,
 * assuming it were to exist.
 */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::lower_bound(const Key& key) {
  return iterator(this, findNodePosition(key));
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_iterator
Treap<Key, Value, Comparator>::lower_bound(const Key& key) const {
  return iterator(this, findNodePosition(key));
}

/* equal_range looks up where the node should be.  If it finds it, it hands
 * back iterators spanning it.  If not, it just hands back two iteators to the
 * same spot.
 */
template <typename Key, typename Value, typename Comparator>
std::pair<typename Treap<Key, Value, Comparator>::iterator,
          typename Treap<Key, Value, Comparator>::iterator>
Treap<Key, Value, Comparator>::equal_range(const Key& key) {
  /* Call lower_bound to find out where we should start looking. */
  std::pair<iterator, iterator> result;
  result.first = result.second = lower_bound(key);

  /* If we hit the end, we're done. */
  if (result.first == end()) return result;

  /* Otherwise, check whether the iterator we found matches the value.  If so,
   * bump the second iterator one step.
   */
  if (!mComp(key, result.second->first)) ++result.second;
  return result;
}
template <typename Key, typename Value, typename Comparator>
std::pair<typename Treap<Key, Value, Comparator>::const_iterator,
          typename Treap<Key, Value, Comparator>::const_iterator>
Treap<Key, Value, Comparator>::equal_range(const Key& key) const {
  /* Call lower_bound to find out where we should start looking. */
  std::pair<const_iterator, const_iterator> result;
  result.first = result.second = lower_bound(key);

  /* If we hit the end, we're done. */
  if (result.first == end()) return result;

  /* Otherwise, check whether the iterator we found matches the value.  If so,
   * bump the second iterator one step.
   */
  if (!mComp(key, result.second->first)) ++result.second;
  return result;
}

/* upper_bound just calls equal_range and returns the second value. */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::iterator
Treap<Key, Value, Comparator>::upper_bound(const Key& key) {
  return equal_range(key).second;
}
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::const_iterator
Treap<Key, Value, Comparator>::upper_bound(const Key& key) const {
  return equal_range(key).second;
}

/* findNodePosition walks the tree looking for the predecessor of the node */
template <typename Key, typename Value, typename Comparator>
typename Treap<Key, Value, Comparator>::Node*
Treap<Key, Value, Comparator>::findNodePosition(const Key& key) const {
  /* If this binary tree were a sorted array, this would work by doing a
   * binary search until we shrunk the range down to one element.  We would
   * then check whether that element were less than the element we were
   * looking for, and if so we would pick the next element.  We do exactly
   * that here, except using the search tree instead of an explicit search.
   */
  Node* curr = mRoot, *prev = NULL;
  while (curr) {
    /* The new prior node is the node itself. */
    prev = curr;

    /* If we're less than the value, go left. */
    if (mComp(key, curr->mValue.first))
      curr = curr->mChildren[0];
    /* If we're greater, go right. */
    else if (mComp(curr->mValue.first, key))
      curr = curr->mChildren[1];
    /* Otherwise, we found the node and can just return it. */
    else
      return curr;
  }

  /* If we got NULL, either the tree is empty or the value compared bigger
   * than everything else.  We should just hand it back.  Also, if the value
   * we got back is bigger than the key, we can hand that back too.
   */
  if (!prev || mComp(key, prev->mValue.first)) return prev;

  /* Otherwise, hand back the value right after it. */
  return prev->mNext;
}

/* Comparison operators == and < use the standard STL algorithms. */
template <typename Key, typename Value, typename Comparator>
bool operator<  (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(),
                                      rhs.begin(), rhs.end());
}
template <typename Key, typename Value, typename Comparator>
bool operator== (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), 
                                                rhs.begin());
}

/* Remaining comparisons implemented in terms of the above comparisons. */
template <typename Key, typename Value, typename Comparator>
bool operator<= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  /* x <= y   iff !(x > y)   iff !(y < x) */
  return !(rhs < lhs);
}
template <typename Key, typename Value, typename Comparator>
bool operator!= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  return !(lhs == rhs);
}
template <typename Key, typename Value, typename Comparator>
bool operator>= (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  /* x >= y   iff !(x < y) */
  return !(lhs < rhs);
}
template <typename Key, typename Value, typename Comparator>

bool operator>  (const Treap<Key, Value, Comparator>& lhs,
                 const Treap<Key, Value, Comparator>& rhs) {
  /* x > y iff y < x */
  return rhs < lhs;
}

#endif
