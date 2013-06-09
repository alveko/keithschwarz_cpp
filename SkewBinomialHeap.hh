/******************************************************************************
 * File: SkewBinomialHeap.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a priority queue backed by a skew binomial heap.  Like
 * regular binomial heaps, skew binomial heaps are forests of specially-shaped
 * trees based on the structure of the numeric representation of the heap size.
 * Unlike regular binomial heaps, which are based on the binary numeral system,
 * skew binomial heaps are based on the skew binary numeral system.  The skew
 * binary numeral system has each digit take the value 2^{k+1} - 1, and each
 * digit can be either 0, 1, or 2.  By convention, numbers are written in a way
 * where the digit 2 must be the least-significant nonzero digit in the
 * representation.  The first few numbers, written in this way, are 0, 1, 2,
 * 10, 12, 20, 100, 101.  This may at first seem like a seemingly arbitrary
 * pattern, but there actually is a deal of method to this madness.  To add one
 * to a skew binary number, look at the last nonzero digit.  If it's a two,
 * then we can use the fact that
 *
 *            2 * (2^{k+1} - 1) + 1 = 2^{k + 2} - 2 + 1 = 2^{k + 2}
 *
 * to remove the 2 digit and then increment the next digit by one.  To see this
 * in action, consider the skew binary number 1011200 = 187 (dec).  We can add
 * one to this number by removing the last 2, then incrementing the 1 that
 * precedes it by one.  This yields 1012000 = 188(dec).  If, on the other hand,
 * the last digit is not 2, then we can add one to the representation while
 * enforcing our invariant by simply incrementing the 1s digit from 0 to 1 or
 * from 1 to 2.  A quick inductive proof can show that this gives the proper
 * mathematical quantity, as well as having all the invariants hold.
 *
 * The advantage of writing numbers in skew binary is that, given a suitable
 * representation for the bits, any number can be incremented in worst-case
 * constant time.  Suppose that we store the number as a list of the nonzero
 * digits, together with tags on each indicating what position those digits
 * are.  Then we can increment by one as follows:
 *
 * - If the last digit is a 2, then remove it.  If the next digit after the 2
 *   is not in the next position, add a new digit with value 1 in the next
 *   position.  Otherwise increment the digit in that position from 1 to 2.
 * - Otherwise, if the last digit is a 1 in the 1s place, increment it to 2.
 * - Otherwise, if the last digit is not in the 1s place, add a 1 in the 1s
 *   place.
 *
 * This logic is a bit complex, but fortunately it can be implemented in time
 * O(1).
 *
 * The idea behind the skew binomial heap is to store a family of heap-ordered
 * trees of different "ranks" in the same way that numbers are written in skew
 * binary.  There can be at most two trees of each rank, and if there are two
 * trees of a given rank then that rank must be the lowest rank.  These trees
 * are shaped in a way that allows them to be merged in one of two ways, either
 * by a regular merge, which takes two trees of the same rank r and produces a
 * new tree of rank r + 1, or by a "skew merge," which takes two trees of rank
 * r and a singleton element, then produces a new tree of rank r + 1 holding
 * both these trees and the new element.
 *
 * The structure of these trees, as developed by Chris Okasaki, is based on a
 * more-or-less straightforward modification of binomial trees.  A skew
 * binomial tree is defined recursively:
 *
 * - A skew binomial tree of order zero is a single node.
 * - A skew binomial tree of order n + 1 is a skew binomial heap of order n
 *   with a skew binomial tree of order n as a child, along with a list of
 *   skew binomial trees of order 0 of length no greater than n + 1.
 *
 * Up to the part about the list of nodes, this description of a skew binomial
 * tree is equivalent to a regular binomial tree.  The difference only shows up
 * when we talk about skew merging.  To do a regular merge on skew binomial
 * trees, we use the same logic for regular binomial tree merges - make the
 * tree with the smaller root the root of the resulting tree, then add the tree
 * with the larger root as a child.  In a skew merge, we do a regular merge
 * first, then consider the new singleton element we're adding.  If the
 * singleton is larger than the root of this new tree, we append it to the list
 * of singleton nodes.  If it is smaller, we swap it with the root of the old
 * tree, then add the root of the old tree as a singleton tree to the list of
 * singleton nodes.
 *
 * This in part explains the limitation of why the list of singleton nodes
 * can't be larger than size n + 1.  Each skew merge adds at most one new node
 * to that list, and so after k skew merges the list can have at most k 
 * elements in it.
 *
 * Notice that because each skew binomial tree can have a list of singleton
 * nodes with a variable number of nodes in it, the number of nodes in a skew
 * binomial tree is not fixed.  However, it does fit in a definitive range.
 * Consider any skew binomial tree of order r.  If none of the nodes in the
 * tree have any nodes in their singleton list, then the tree has the same size
 * and shape of a regular binomial tree, and thus has 2^r nodes.  If, on the
 * other hand, every node has the maximum number of singleton nodes, we can
 * prove by induction that the tree has 2^r - 1 additional nodes, for a total
 * of 2^{r + 1} - 1 nodes.  The proof is as follows.  As a base case, a skew
 * binomial tree of order 0 has no extra nodes, and 2^0 - 1 = 1 - 1 = 0.  For
 * the inductive step, assume that for all trees of order n' < n, the maximum
 * number of extra nodes in the tree (denoted E(r)) is 2^n' - 1.  Then we have
 * that
 *          n-1
 * E(n) = ( sum  E(i) ) + n
 *         i = 0
 *
 * This follows because the skew binomial tree of order n has children of order
 * 0, 1, ..., n - 1, each of which have the maximum number of nodes, and itself
 * carries n extra nodes.  Using the inductive hypothesis and rearranging, we
 * get
 *
 *
 *          n-1                 n-1
 * E(n) = ( sum  E(i) ) + n = ( sum  2^i - 1) + n
 *         i = 0               i = 0
 *
 *                              n-1
 *                          = ( sum 2^i ) - n + n
 *                             i = 0
 *
 *                              n-1
 *                          = ( sum 2^i ) = 2^n - 1
 *                             i = 0
 *
 * This last step follows from a well-known and easily verified result.
 *
 * The net result of this proof is that we can bound the number of nodes in a
 * skew binomial tree of order n by the range [2^n, 2^{n + 1}).  This means
 * that for any number k, if we partition the elements of k into a forest of
 * skew binomial trees allowing for no more than two trees of each order, there
 * are at most O(lg k) such trees, each of which has order at most O(lg k).
 *
 * Now that we know the shape of these trees and the way in which they are
 * merged and skew-merged, we can start discussing basic operations of a skew
 * binomial heap made of a forest of these trees.  The heap will be a forest of
 * skew binomial trees with the following restrictions:
 *
 * - Each tree is heap-ordered.
 * - There are at most two trees of any order.
 * - If there are two trees of the same order, that is the lowest order of the
 *   trees.
 *
 * Note the similarity between this definition and the way in which we write
 * skew binary numbers.
 *
 * To find the minimum element of the skew binomial heap, we simply scan over
 * all the heaps looking for the minimum element; this can be done in O(lg n)
 * time, since there are at most O(lg n) trees.  To insert an element, we look
 * at the two lowest-order trees in the heap.  If they have the same order, we
 * skew merge them together into a tree of the next-highest order.  If not,
 * then we add the new element as a tree of order 0.
 *
 * To merge two skew binomial heaps, we reduce the problem to one akin to a
 * standard binomial heap merge.  First, we ensure that each heap has at most
 * tree of each order by merging together any two trees with the same order,
 * then doing a standard binomial heap "ripple-carry" operation to ensure that
 * there is at most one tree of each order.  This takes time O(lg n), since
 * there are at most O(lg n) trees.  Finally, we merge the two lists of trees
 * together using the same logic as for binomial heaps by mimicking binary
 * addition.  This also takes O(lg n) time, since there are at most O(lg n)
 * trees.
 *
 * To dequeue the minimum element, we locate the minimum element as before and
 * remove it to expose a forest of skew binomial trees, along with a list of
 * singleton trees.  We then merge together the forest of trees with the top
 * list of trees, which takes O(lg n) time since there are O(lg n) top-level
 * trees and O(lg n) exposed trees, since if the tree whose min was removed had
 * order r, there are r - 1 trees, and the maximum possible order is O(lg n).
 * Finally, we call insert once for each singleton tree, and since there are at
 * most O(lg n) of them and each insert takes O(1), this step completes in
 * O(lg n) for a net runtime of O(lg n) for the dequeue step.
 */

#ifndef SkewBinomialHeap_Included
#define SkewBinomialHeap_Included

#include <functional> // For less
#include <algorithm>  // For for_each
#include <cassert>
#include <deque>
#include <vector>

/**
 * A priority queue implemented as a skew binomial heap.  The queue always
 * provides access to the smallest element it contains.
 */
template <typename T, typename Comparator = std::less<T> >
class SkewBinomialHeap {
public:
  /**
   * Constructor: SkewBinomialHeap();
   * Usage: SkewBinomialHeap<int> myHeap;
   * --------------------------------------------------------------------------
   * Constructs a new, empty SkewBinomialHeap.
   */
  explicit SkewBinomialHeap(Comparator comp = Comparator());

  /**
   * Destructor: ~SkewBinomialHeap();
   * Usage: (implicit)
   * --------------------------------------------------------------------------
   * Deallocates the skew binomial heap, freeing all allocated memory.
   */
  ~SkewBinomialHeap();

  /**
   * Copy functions: SkewBinomialHeap(const SkewBinomialHeap& rhs);
   *                 SkewBinomialHeap& operator= (const SkewBinomialHeap& rhs);
   * Usage: SkewBinomialHeap<int> clone = extant;
   *                              clone = extant;
   * --------------------------------------------------------------------------
   * Deep-copies this skew binomial heap.
   */
  SkewBinomialHeap(const SkewBinomialHeap& rhs);
  SkewBinomialHeap& operator= (const SkewBinomialHeap& rhs);

  /**
   * void push(const T& toAdd);
   * Usage: sbh.add(137);
   * --------------------------------------------------------------------------
   * Adds a new element to the skew binomial heap.
   */
  void push(const T& toAdd);

  /**
   * void pop();
   * Usage: sbh.pop();
   * --------------------------------------------------------------------------
   * Removes, but does not return, the minimum element of the skew binomial
   * heap.  If the heap is empty, the behavior is undefined.
   */
  void pop();

  /**
   * const T& top() const;
   * Usage: cout << sbh.top() << endl;
   * --------------------------------------------------------------------------
   * Returns a reference to the smallest element in the heap.  This reference
   * is only guaranteed to be valid until the next call to push() or pop(),
   * or merge(), and becomes invalid if this object is merged with another.  If
   * the heap is empty, the behavior is undefined.
   */
  const T& top() const;

  /**
   * void merge(SkewBinomialTree& other);
   * Usage: lhs.merge(rhs);
   * --------------------------------------------------------------------------
   * Updates this object to contain all of its elements, plus all of the
   * elements in the other skew binomial heap.  The argument is destructively
   * modified to be empty upon exit.
   */
  void merge(SkewBinomialHeap& other);

  /**
   * size_t size() const;
   * Usage: size_t s = sbh.size();
   * --------------------------------------------------------------------------
   * Returns the number of elements in the skew binomial heap.
   */
  size_t size() const;

  /**
   * bool empty() const;
   * Usage: while (!sbh.empty()) { ... }
   * --------------------------------------------------------------------------
   * Returns whether this skew binomial heap is empty.
   */
  bool empty() const;

  /**
   * void swap(SkewBinomialHeap& other);
   * Usage: lhs.swap(rhs);
   * --------------------------------------------------------------------------
   * Exchanges the contents of this skew binomial heap with some other skew
   * binomial heap in O(1).
   */
  void swap(SkewBinomialHeap& other);

private:
  /* A utility class representing a skew binomial tree. */
  struct Tree {
    size_t            mOrder;      // The order of the tree.
    std::deque<Tree*> mChildren;   // Its children, in ascending size order.
    std::deque<Tree*> mSingletons; // The singleton nodes that live here.
    T                 mValue;      // The value stored in this node.

    /**
     * Utility constructor: Tree(const T& value, size_t order);
     * Usage: new Tree(value, 2);
     * ------------------------------------------------------------------------
     * Constructs a new skew binomial tree of the specified order and with the
     * indicated value.
     */
    Tree(const T& value, size_t order) : mOrder(order), mValue(value) {
      // Handled in initializer list.
    }
  };

  /* The trees in this heap. */
  std::deque<Tree*> mTrees;

  /* A cached count of the number of elements here. */
  size_t mSize;

  /* The comparator to use here. */
  Comparator mComp;

  /* Utility function to free a single tree. */
  static void freeTree(Tree* toFree);

  /* Utility function that inserts a singleton tree into the tree.  The size is
   * not updated, but the underlying tree structure is.
   */
  void insertSingleton(Tree* singleton);

  /* Utility function that given two trees performs a standard merge on those
   * trees to produce a new tree.
   */
  Tree* mergeTrees(Tree* first, Tree* second);

  /* Utility function that, given two list of trees, merges those trees into
   * one sorted list.  More specifically, the list of trees of the first two
   * arguments are merged together.  The first list of trees holds the result,
   * while the second list of trees is emptied.
   */
  void mergeHeaps(std::deque<Tree*>& lhs, std::deque<Tree*>& rhs);

  /* Utility function that finds and returns the index of the smallest element
   * in the list of trees.
   */
  size_t indexOfMin() const;

  /* Utility function that clones a skew binomial tree and returns the new
   * tree.
   */
  static Tree* cloneTree(const Tree* toClone);
};

/* * * * * Implementation Below This Point * * * * */

/* Constructor stores the indicated comparator. */
template <typename T, typename Comparator>
SkewBinomialHeap<T, Comparator>::SkewBinomialHeap(Comparator comp)
  : mSize(0), mComp(comp) {
  // Handled in initializer list.
}


/* Destructor walks over the trees, recursively deallocating them. */
template <typename T, typename Comparator>
SkewBinomialHeap<T, Comparator>::~SkewBinomialHeap() {
  std::for_each(mTrees.begin(), mTrees.end(), &SkewBinomialHeap::freeTree);
}

/* Freeing a tree involves freeing its singletons and its children. */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::freeTree(Tree* toFree) {
  /* If the there is no tree, don't free anything. */
  if (!toFree) return;

  /* Free the tree's singletons. */
  std::for_each(toFree->mSingletons.begin(), toFree->mSingletons.end(),
                &SkewBinomialHeap::freeTree);

  /* Free the tree's children. */
  std::for_each(toFree->mChildren.begin(), toFree->mChildren.end(),
                &SkewBinomialHeap::freeTree);

  /* Free the node itself. */
  delete toFree;
}

/* Size queries return the cached size. */
template <typename T, typename Comparator>
size_t SkewBinomialHeap<T, Comparator>::size() const {
  return mSize;
}

/* Emptiness queries return whether the size is zero. */
template <typename T, typename Comparator>
bool SkewBinomialHeap<T, Comparator>::empty() const {
  return size() == 0;
}

/* Adding an element to the tree wraps it up in a singleton, then merges it
 * into the tree.
 */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::push(const T& value) {
  /* Wrap the object up in a new singleton tree and add it. */
  insertSingleton(new Tree(value, 0));

  /* Increment our cached copy of the size so we track the number of elements
   * correctly.
   */
  ++mSize;
}

/* Inserting a singleton tree follows the rules specified in the file comments
 * to combine the new singleton with the existing trees.
 */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::insertSingleton(Tree* toAdd) {
  assert (toAdd && toAdd->mOrder == 0);

  /* If the last two trees have the same order, merge them together into one
   * tree using a skew merge.
   */
  if (mTrees.size() >= 2 && mTrees[0]->mOrder == mTrees[1]->mOrder) {
    /* First, remove both trees from the tree list; we're going to be replacing
     * them with one new tree.
     */
    Tree* first  = mTrees.front(); mTrees.pop_front();
    Tree* second = mTrees.front(); mTrees.pop_front();
    
    /* Link the trees using a standard merge. */
    Tree* newTree = mergeTrees(first, second);

    /* Now, if the singleton tree has a value less than that of the new union,
     * swap its value with the value atop the tree.
     */
    if (mComp(toAdd->mValue, newTree->mValue))
      std::swap(toAdd->mValue, newTree->mValue);
    
    /* Make the singleton (which might now be holding a value other than the
     * one it started with) an entry in the resulting singleton list.
     */
    newTree->mSingletons.push_back(toAdd);

    /* Finally, put this tree at the front of the list of trees. */
    mTrees.push_front(newTree);
  }
  /* Otherwise, just prepend the tree to the front of the list; we have nothing
   * to merge.
   */
  else {
    mTrees.push_front(toAdd);
  }
}

/* Merging two trees makes the tree with the smaller root the parent of the
 * other tree, then increases its order.
 */
template <typename T, typename Comparator>
typename SkewBinomialHeap<T, Comparator>::Tree* 
SkewBinomialHeap<T, Comparator>::mergeTrees(Tree* one, Tree* two) {
  assert (one && two);

  /* Make sure that the first tree has a lower value, and exchange the two if
   * that isn't the case so we can pretend it was all along.
   */
  if (mComp(two->mValue, one->mValue))
    std::swap(one, two);

  /* Make two the last child of one. */
  one->mChildren.push_back(two);

  /* Increase the order of the first tree, since it now has another child. */
  ++one->mOrder;

  /* Hand back this tree as the new root of the merger. */
  return one;
}

/* Locating the minimum element works by scanning the trees for the lowest 
 * value and handing back a reference to it.
 */
template <typename T, typename Comparator>
const T& SkewBinomialHeap<T, Comparator>::top() const {
  assert (!empty());
  return mTrees[indexOfMin()]->mValue;
}

/* To find the minimum element, we scan the roots of all the trees. */
template <typename T, typename Comparator>
size_t SkewBinomialHeap<T, Comparator>::indexOfMin() const {
  /* Guess that the smallest element is in the first tree. */
  int min = 0;

  /* Update this guess. */
  for (size_t i = 1; i < mTrees.size(); ++i)
    if (mComp(mTrees[i]->mValue, mTrees[min]->mValue))
      min = i;

  /* Hand back a reference to the value. */
  return min;
}

/* Merge operation forwards the request to a helper function which does the
 * actual logic.
 */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::merge(SkewBinomialHeap& other) {
  /* Do the actual logic necessary to merge the heaps together. */
  mergeHeaps(mTrees, other.mTrees);

  /* Update the size information of both trees; this heap just grew in size,
   * while the other heap lost all its elements.
   */
  mSize += other.mSize;
  other.mSize = 0;
}

/* Given two lists of trees, merges them together into a single list of trees
 * holding all the original elements.
 */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::mergeHeaps(std::deque<Tree*>& lhs,
                                                 std::deque<Tree*>& rhs) {
  /* Merge all of the trees from the two input sequences together by their
   * length.  We'll use this to simplify the processing logic.
   */
  std::deque<Tree*> allTrees;
  while (!lhs.empty() && !rhs.empty()) {
    if (lhs.front()->mOrder < rhs.front()->mOrder) {
      allTrees.push_back(lhs.front()); lhs.pop_front();
    } else {
      allTrees.push_back(rhs.front()); rhs.pop_front();
    }
  }

  /* Add the remaining heaps to the list as well.  Only one of these two
   * branches will execute, so this maintains the sorted order.
   */
  allTrees.insert(allTrees.end(), lhs.begin(), lhs.end());
  allTrees.insert(allTrees.end(), rhs.begin(), rhs.end());
  lhs.clear();  rhs.clear();

  /* The merge process is similar to binary addition.  We iterate across the
   * trees, at each point looking at all the trees of a certain order.  If
   * there's exactly one tree of each size, then we just put that in the output
   * list.  If there are two, we put nothing, then store the merge of the tree
   * as a "carry."  Finally, if there are three, we store one, then use the
   * merge of the other two as a carry.
   *
   * The difference from regular binary addition that we have to consider is
   * that in a skew binomial heap there might be multiple trees of the same
   * order.  In the worst case, we'll have four different trees of the same
   * order at any one time (since there's at most two trees of the same order
   * in each tree).  Consequently, we'll generalize the logic a bit.  For each
   * two trees of a given order that we find, we'll merge them together into
   * a tree of the next highest-order.  Any odd tree remaining is left in the
   * output sequence.
   */
  while (!allTrees.empty()) {
    /* Populate a buffer with trees that have the same order as the first tree
     * left in the worklist.  Initially this just has the first tree.
     */
    std::deque<Tree*> ofSameOrder;
    ofSameOrder.push_back(allTrees.front()); allTrees.pop_front();

    /* Move all other trees into this list with the same order. */
    while (!allTrees.empty() && 
           allTrees.front()->mOrder == ofSameOrder.front()->mOrder) {
      ofSameOrder.push_back(allTrees.front()); allTrees.pop_front();
    }

    /* If we have an odd number of trees, write one of them to the output. */
    if (ofSameOrder.size() % 2 == 1) {
      lhs.push_back(ofSameOrder.front()); ofSameOrder.pop_front();
    }
    assert (ofSameOrder.size() % 2 == 0);
    
    /* Keep merging pairs of remaining trees and putting them back in the 
     * worklist. 
     */
    for (size_t i = 0; i < ofSameOrder.size(); i += 2)
      allTrees.push_front(mergeTrees(ofSameOrder[i], ofSameOrder[i + 1]));
  }
}

/* Removing the min element requires removing it from the list of trees, then
 * adding the child trees and singletons back in.
 */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::pop() {
  assert (!empty());

  /* Track down the index of the smallest tree, then remove it from the
   * list of trees.
   */
  const size_t minIndex = indexOfMin();
  Tree* toRemove = mTrees[minIndex];
  mTrees.erase(mTrees.begin() + minIndex);

  /* Now, we need to merge all of the child trees back in to the master
   * list.
   */
  mergeHeaps(mTrees, toRemove->mChildren);

  /* For each of the singletons held by this tree, insert that singleton back
   * into the tree.
   */
  for (size_t i = 0; i < toRemove->mSingletons.size(); ++i)
    insertSingleton(toRemove->mSingletons[i]);

  /* Decrement the total number of trees remaining here; we're about to remove
   * something.
   */
  --mSize;

  /* Free the memory associated with this entry. */
  delete toRemove;
}

/* Cloning a tree recursively copies all of the subtrees. */
template <typename T, typename Comparator>
typename SkewBinomialHeap<T, Comparator>::Tree*
SkewBinomialHeap<T, Comparator>::cloneTree(const Tree* toClone) {
  /* The clone of the empty tree is the empty tree. */
  if (!toClone) return NULL;

  /* Otherwise, clone the tree's value and order. */
  Tree* result = new Tree(toClone->mValue, toClone->mOrder);

  /* Deep-copy the children and singletons. */
  for (size_t i = 0; i < toClone->mChildren.size(); ++i)
    result->mChildren.push_back(cloneTree(toClone->mChildren[i]));
  for (size_t i = 0; i < toClone->mSingletons.size(); ++i)
    result->mSingletons.push_back(cloneTree(toClone->mSingletons[i]));

  /* Hand back the copied tree. */
  return result;
}

/* To deep-copy a skew binomial heap, we copy each tree and the cached value
 * of the number of nodes present.
 */
template <typename T, typename Comparator>
SkewBinomialHeap<T, Comparator>::SkewBinomialHeap(const SkewBinomialHeap& rhs) {
  /* Clone all the trees in order. */
  for (size_t i = 0; i < rhs.mTrees.size(); ++i)
    mTrees.push_back(cloneTree(rhs.mTrees[i]));

  /* Copy the size over so we know how many elements are here. */
  mSize = rhs.mSize;
  mComp = rhs.mComp;
}

/* To exchange two skew binomial heaps, we just swap the trees and size. */
template <typename T, typename Comparator>
void SkewBinomialHeap<T, Comparator>::swap(SkewBinomialHeap& rhs) {
  std::swap(mSize, rhs.mSize);
  std::swap(mComp, rhs.mComp);
  mTrees.swap(rhs.mTrees);
}

/* Assignment implemented with copy-and-swap. */
template <typename T, typename Comparator>
SkewBinomialHeap<T, Comparator>&
SkewBinomialHeap<T, Comparator>::operator= (const SkewBinomialHeap& rhs) {
  SkewBinomialHeap copy = rhs;
  swap(copy);
  return *this;
}

#endif
