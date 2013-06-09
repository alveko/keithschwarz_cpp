/*****************************************************************************
 * File: VanEmdeBoasTree.cpp
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of the VanEmdeBoasTree class.
 */

#include "VanEmdeBoasTree.hh"

/**** Module constants ****/

/* A utility constant holding the number of bits in an unsigned short. */
const size_t kShortBits = sizeof(unsigned short) * CHAR_BIT;

/* A utility constaint holding the number of bits before the Node
 * representation switches from a standard vEB-tree structure to a bitvector.
 * We'll pick four bits as our cutoff, since this lets the result fit into a
 * 32-bit long.
 */
const size_t kBitvectorSize = 4;

/**** Utility functions ****/

/* Function which, given an unsigned short and a number of bits, returns the
 * lower half of those bits.
 */
static unsigned short LowerBits(unsigned short value, size_t numBits) {
  /* To recover the lower bits, we'll compute 2^(numBits/2) - 1.  This value's
   * binary representation is 00..0011..11, where the number of ones is given
   * by numBits / 2.  We can then AND this with the original value to get the
   * result.
   */
  return value & (unsigned short)((1 << (numBits / 2)) - 1);
}

/* Function which, given an unsigned short and a number of bits, returns the
 * upper half of those bits.
 */
static unsigned short UpperBits(unsigned short value, size_t numBits) {
  /* This is given by the original number shifted down numBits / 2 positions. */
  return value >> (numBits / 2);
}

/* Function which, given two unsigned shorts representing the lower and upper
 * bits of a value, returns that value.
 */
static unsigned short Compose(unsigned short upper, unsigned short lower,
                              size_t numBits) {
  return (upper << numBits) + lower;
}

/**** Implementation of Node. ****/

/* operator new takes in a number of pointers, then overallocates space for
 * those pointers.
 */
void* VanEmdeBoasTree::Node::operator new(size_t size, size_t numPointers) {
  /* The space we need is computed as follows:
   *
   * 1. We need at least size space for the base array.
   * 2. We need numPointers - 1 extra pointers.
   */
  return ::operator new(size + sizeof(Node*) * (numPointers - 1));
}

/* operator delete doesn't do anything fancy; it just forwards the call to the
 * global operator delete.  Since we don't need the second argument, we don't
 * name it.
 */
void VanEmdeBoasTree::Node::operator delete(void* memory, size_t) {
  ::operator delete(memory);
}

/**** Implementation of const_iterator ****/

/* Default constructor sets the current value to the sentinel. */
VanEmdeBoasTree::const_iterator::const_iterator() {
  mCurr = VanEmdeBoasTree::kNil;

  /* No one owns this iterator. */
  mOwner = NULL;
}

/* Parameterized constructor sets the current value to the indicated value. */
VanEmdeBoasTree::const_iterator::const_iterator(size_t valueOrSentinel,
                                                const VanEmdeBoasTree* owner) {
  mCurr = valueOrSentinel;
  mOwner = owner;
}

/* Equality checks for equality of the underlying value and tree. */
bool VanEmdeBoasTree::const_iterator::operator== (const const_iterator& rhs) const {
  return mCurr == rhs.mCurr && mOwner == rhs.mOwner;
}

/* Disequality implemented in terms of equality. */
bool VanEmdeBoasTree::const_iterator::operator!= (const const_iterator& rhs) const {
  return !(*this == rhs);
}

/* Pointer dereference just hands back the stored value.  Note that if our
 * value is the sentinel this hands back something pretty much random, but
 * that's okay because the clients should't be dereferencing it in the first
 * place.
 */
const unsigned short VanEmdeBoasTree::const_iterator::operator* () const {
  return static_cast<unsigned short>(mCurr);
}

/* Advance operator works by updating this iterator to the successor of the
 * current value.
 */
VanEmdeBoasTree::const_iterator& 
VanEmdeBoasTree::const_iterator::operator ++() {
  /* Ask the owner for the successor. */
  *this = mOwner->successor(mCurr);
  return *this;
}

/* Postfix ++ implemented in terms of prefix ++. */
const VanEmdeBoasTree::const_iterator
VanEmdeBoasTree::const_iterator::operator++ (int) {
  const_iterator result = *this; // Cache value...
  ++*this;                       // ... advance ...
  return result;                 // ... and return cached value.
}

/* Retreat operator works by updating this iterator to be the predecessor of
 * the current value.
 */
VanEmdeBoasTree::const_iterator& 
VanEmdeBoasTree::const_iterator::operator --() {
  /* Special case: If we are one step past the end of the range, we are still
   * allowed to back up.  This gives an iterator to the maximum element of the
   * tree.
   */
  if (mCurr == VanEmdeBoasTree::kNil) {
    mCurr = VanEmdeBoasTree::treeMax(mOwner->mRoot, kShortBits);
  } 
  /* Otherwise, just ask the owner for the predecessor. */
  else {
    *this = mOwner->predecessor(mCurr);
  }
  return *this;
}

/* Postfix -- implemented in terms of prefix --. */
const VanEmdeBoasTree::const_iterator
VanEmdeBoasTree::const_iterator::operator-- (int) {
  const_iterator result = *this; // Cache value...
  --*this;                       // ... back up ...
  return result;                 // ... and return cached value.
}

/**** Implementation of VanEmdeBoasTree interface. */

/* Constructor recursively constructs all of the tree structure. */
VanEmdeBoasTree::VanEmdeBoasTree() {
  /* Initially, the tree is empty. */
  mSize = 0;

  /* Build up a tree with sufficiently many bits to hold any unsigned short */
  mRoot = recCreateTree(kShortBits);
}

/* Copy constructor recursively clones the other tree. */
VanEmdeBoasTree::VanEmdeBoasTree(const VanEmdeBoasTree& other) {
  /* Copy size information. */
  mSize = other.mSize;

  /* Recursively clone the other tree. */
  mRoot = recCloneTree(other.mRoot, kShortBits);
}

/* Destructor recursively deletes the tree structure. */
VanEmdeBoasTree::~VanEmdeBoasTree() {
  recDeleteTree(mRoot, kShortBits);
}

/* Assignment operator implemented using copy-and-swap. */
VanEmdeBoasTree& VanEmdeBoasTree::operator= (const VanEmdeBoasTree& other) {
  VanEmdeBoasTree copy = other;
  swap(copy);
  return *this;
}

/* begin returns a const_iterator to the smallest value in the tree. */
VanEmdeBoasTree::const_iterator VanEmdeBoasTree::begin() const {
  return const_iterator(treeMin(mRoot, kShortBits), this);
}

/* end returns a const_iterator to the sentinel value. */
VanEmdeBoasTree::const_iterator VanEmdeBoasTree::end() const {
  return const_iterator(kNil, this);
}

/* Reverse begin and end functions just wrap up end and begin, respectively. */
VanEmdeBoasTree::const_reverse_iterator VanEmdeBoasTree::rbegin() const {
  return const_reverse_iterator(end());
}
VanEmdeBoasTree::const_reverse_iterator VanEmdeBoasTree::rend() const {
  return const_reverse_iterator(begin());
}

/* size hands back the cached size. */
size_t VanEmdeBoasTree::size() const {
  return mSize;
}

/* empty reports whether the size is zero. */
bool VanEmdeBoasTree::empty() const {
  return size() == 0;
}

/* find recursively searches the tree for the specified value.  If it's found,
 * the function returns a valid iterator that wraps the value.  Otherwise, it
 * returns end() as a sentinel.
 */
VanEmdeBoasTree::const_iterator VanEmdeBoasTree::find(unsigned short value) const {
  return recFindElement(value, mRoot, kShortBits)? const_iterator(value, this) : end();
}

/* insert recursively inserts a value into the tree, returning an iterator to
 * it and flagging whether or not it was found.
 */
std::pair<VanEmdeBoasTree::const_iterator, bool>
VanEmdeBoasTree::insert(unsigned short value) {
  /* Recursively insert the element into the tree. */
  const bool didInsert = recInsertElement(value, mRoot, kShortBits);

  /* If the value was inserted, bump up the total number of elements we store
   * in the tree.
   */
  if (didInsert) ++mSize;

  /* Hand back a pair of an iterator to the value and whether it was added. */
  return std::make_pair(const_iterator(value, this), didInsert);
}

/* Erasing an element just forwards the call to the recursive delete procedure. */
bool VanEmdeBoasTree::erase(unsigned short value) {
  /* Wipe the element from the tree. */
  const bool result = recEraseElement(value, mRoot, kShortBits);

  /* If something was removed, drop our effective size. */
  if (result) --mSize;

  return result;
}

/* Erasing an iterator just recovers the underlying value from the iterator
 * and uses it as a target for erasure.
 */
bool VanEmdeBoasTree::erase(const_iterator where) {
  return erase(where.mCurr);
}

/* successor and predecessor just wrap the result of the recursive calls. */
VanEmdeBoasTree::const_iterator 
VanEmdeBoasTree::successor(unsigned short value) const {
  return const_iterator(recSuccessor(value, mRoot, kShortBits), this);
}
VanEmdeBoasTree::const_iterator 
VanEmdeBoasTree::predecessor(unsigned short value) const {
  return const_iterator(recPredecessor(value, mRoot, kShortBits), this);
}

/* swap simply exchanges data members with the other tree. */
void VanEmdeBoasTree::swap(VanEmdeBoasTree& other) {
  std::swap(mSize, other.mSize);
  std::swap(mRoot, other.mRoot);
}

/**** Implementation of private helper functions for VanEmdeBoasTree ****/

/* To recursively create a tree, we look at the number of remaining bits.  If
 * it's sufficiently small, we use a bitvector.  Otherwise, we create a new
 * Node object and fill its fields in recursively.
 */
void* VanEmdeBoasTree::recCreateTree(size_t numBits) {
  /* If we're below the cutoff, allocate a new long (32 bits) whose bits are
   * all zero.
   */
  if (numBits <= kBitvectorSize)
    return new long(0);
  
  /* Compute how many pointers we'll need.  This is 2^(numBits / 2). */
  const size_t numPointers = 1 << (numBits / 2);

  /* Otherwise, allocate a node and fill the fields in. */
  Node* result = new (numPointers) Node;

  /* The node is initially empty. */
  result->mIsEmpty = true;

  /* Create a summary structure to hold numBits / 2 bits. */
  result->mSummary = recCreateTree(numBits / 2);

  /* Each of the result's pointers is a vEB-tree of numBits / 2 bits. */
  for (size_t i = 0; i < numPointers; ++i)
    result->mChildren[i] = recCreateTree(numBits / 2);

  return result;
}

/* Recursively destroying a tree involves scanning over that tree's pointers
 * and freeing them.
 */
void VanEmdeBoasTree::recDeleteTree(void* root, size_t numBits) {
  /* If the number of bits is below the cutoff, deallocate the long that we
   * allocated.
   */
  if (numBits <= kBitvectorSize) {
    delete static_cast<long*>(root);
    return;
  }

  /* Otherwise, this is a node and we need to free its fields. */
  Node* node = static_cast<Node*>(root);

  /* Deallocate the summary structure. */
  recDeleteTree(node->mSummary, numBits / 2);

  /* Compute the number of pointers; again this is 2^(numBits / 2). */
  const size_t numPointers = 1 << (numBits / 2);

  /* Wipe out the subtrees. */
  for (size_t i = 0 ; i < numPointers; ++i)
    recDeleteTree(node->mChildren[i], numBits / 2);

  /* Finally, free the node itself. */
  delete node;
}

/* Recursively scanning for an element involves descending into the proper
 * tree looking for the value in question.
 */
bool VanEmdeBoasTree::recFindElement(unsigned short value, void* root, 
                                     size_t numBits) {
  /* If the number of bits is low enough that we're looking at a bitvector,
   * just test whether the appropriate bit is set.
   */
  if (numBits <= kBitvectorSize)
    return (*static_cast<long*>(root) & (1 << value)) != 0;

  /* Otherwise, this is a real node. */
  Node* node = static_cast<Node*>(root);

  /* If this node is empty, the element can't be here. */
  if (node->mIsEmpty) return false;

  /* Otherwise, check if this value is the min or max. */
  if (value == node->mMin || value == node->mMax) return true;

  /* If it's neither of these, descend into the proper subtree looking for the
   * lower half of the bits.
   */
  return recFindElement(LowerBits(value, numBits),
                        node->mChildren[UpperBits(value, numBits)],
                        numBits / 2);
}

/* Inserting an element walks down the tree, putting the proper value in the
 * proper place and updating the summary structure.
 */
bool VanEmdeBoasTree::recInsertElement(unsigned short value, void* root, 
                                       size_t numBits) {
  /* First, if we're dealing with a bitvector implementation, just set the
   * appropriate bit.
   */
  if (numBits <= kBitvectorSize) {
    /* The bitvector is really a long, so get a handle to it. */
    long& bitvector = *static_cast<long*>(root);
    
    /* If the bit at the proper position is already set, return false to
     * signal that we didn't insert anything.
     */
    if (bitvector & (1 << value)) return false;
    
    /* Set the bit at position value. */
    bitvector |= (1 << value);
    return true;
  }

  /* Otherwise, what we have here is a real node. */
  Node* node = static_cast<Node*>(root);

  /* If this node is empty, then we insert the value by setting it as the only
   * value here.
   */
  if (node->mIsEmpty) {
    /* Set the minimum and maximum value to this value, since it's the only
     * value in the entire structure.
     */
    node->mMin = node->mMax = value;

    /* Mark the node as being nonempty. */
    node->mIsEmpty = false;

    /* We added something, since nothing was initially here. */
    return true;
  }

  /* Otherwise, if the value matches either the min or the max, we're done. */
  if (value == node->mMin || value == node->mMax)
    return false;

  /* Otherwise, if both the min and max are the same value, then the node has
   * only one value and the new one will become either the min or the max.
   */
  if (node->mMin == node->mMax) {
    const unsigned short min = std::min(node->mMin, value);
    const unsigned short max = std::max(node->mMax, value);
    node->mMin = min;
    node->mMax = max;
    return true;
  }

  /* Otherwise, we are dealing with a node that already has a min and max set,
   * where neither of those values matches the value we're inserting.  In this
   * case, we will do one of three things:
   *
   * 1. Fall in the range (min, max), and be inserted into some subtree.
   * 2. Be less than the min, displacing the min and inserting it into some
   *    subtree.
   * 3. Be greater than the max, displacing the max and inserting it into some
   *    subtree.
   *
   * To unify the cases, we'll update the min and max appropriately before
   * recursively inserting the value further down into the tree.
   */
  if (value < node->mMin)
    std::swap(value, node->mMin);
  if (value > node->mMax)
    std::swap(value, node->mMax);

  /* This next step is tricky.  When inserting this next value recursively, we
   * will descend into one of the subtrees.  If it's empty, then we will add
   * the upper bits to the summary tree to indicate that the tree is no longer
   * empty.  If not, then it's already in the summary.  The reason for doing
   * this somewhat tricky check is to get the runtime working better.  Since
   * inserting into an empty tree is fast (it just sets a value and returns),
   * of the two necessary recursive calls (one for the upper and one for the
   * lower), one call runs in O(1).  The other call runs normally, and so the
   * recurrence relation for the runtime only requires one recursive call.
   */
  unsigned short nextTree = UpperBits(value, numBits);
  if (isTreeEmpty(node->mChildren[nextTree], numBits / 2))
    recInsertElement(nextTree, node->mSummary, numBits / 2);

  /* In either case, recursively insert the value into the proper subtree.
   * This might immediately return, but it's still necessary.
   */
  return recInsertElement(LowerBits(value, numBits), 
                          node->mChildren[nextTree],
                          numBits / 2);
}

/* Obtaining the maximum or minimum value from a tree depends on whether the
 * tree is a bitvector or not.
 */
size_t VanEmdeBoasTree::treeMax(void* root, size_t numBits) {
  /* If the tree is a bitvector, march down the bits checking where the
   * largest is.
   */
  if (numBits <= kBitvectorSize) {
    /* For convenience. */
    const long bitvector = *static_cast<long*>(root);

    /* The largest bit index in this bitvector is 2^numBits - 1.  We'll start
     * there and march backwards until we hit something.
     */
    for (int index = (1 << numBits) - 1; index >= 0; --index)
      if (bitvector & (1 << index)) 
        return index;

    /* If we got here we didn't find anything, and so we should return the
     * sentinel value.
     */
    return kNil;
  }

  /* Otherwise what we're looking at is a real node. */
  Node* node = static_cast<Node*>(root);

  /* If the node is empty, it has no maximum value.  Otherwise, it's the
   * node's stated maximum.
   */
  return node->mIsEmpty? kNil : node->mMax;
}

/* The case for the minimum is symmetric. */
size_t VanEmdeBoasTree::treeMin(void* root, size_t numBits) {
  /* If the tree is a bitvector, march down the bits checking where the
   * smallest is.
   */
  if (numBits <= kBitvectorSize) {
    /* For convenience. */
    const long bitvector = *static_cast<long*>(root);

    /* The largest bit index in this bitvector is 2^numBits - 1.  We'll start
     * at zero and count up to it.
     */
    for (int index = 0; index < (1 << numBits); ++index)
      if (bitvector & (1 << index)) 
        return index;

    /* If we got here we didn't find anything, and so we should return the
     * sentinel value.
     */
    return kNil;
  }

  /* Otherwise what we're looking at is a real node. */
  Node* node = static_cast<Node*>(root);

  /* If the node is empty, it has no minimum value.  Otherwise, it's the
   * node's stated minimum.
   */
  return node->mIsEmpty? kNil : node->mMin;
}

/* Determining whether a tree is empty is fairly easy, but depends on the type
 * of tree.
 */
bool VanEmdeBoasTree::isTreeEmpty(void* root, size_t numBits) {
  /* If this is a bitvector, the tree is empty if the bitvector is identically
   * zero.
   */
  if (numBits <= kBitvectorSize)
    return *static_cast<long*>(root) == 0L;

  /* Otherwise, the tree is empty if it's marked as such. */
  return static_cast<Node*>(root)->mIsEmpty;
}

/* Deleting an element is tricky and depends on what type of object we're
 * deleting from.
 */
bool VanEmdeBoasTree::recEraseElement(unsigned short value, void* root,
                                      size_t numBits) {
  /* If we're in bitvector mode, just clear the appropriate bit. */
  if (numBits <= kBitvectorSize) {
    /* Get a handle on the bitvector itself. */
    long& bitvector = *static_cast<long*>(root);

    /* If the bit is not yet set, report that we didn't remove anything. */
    if ((bitvector & (1 << value)) == 0)
      return false;
    
    /* Otherwise, clear the bit. */
    bitvector &= ~(1 << value);
    return true;
  }

  /* Otherwise, this is a real node. */
  Node* node = static_cast<Node*>(root);

  /* If this node has nothing in it, then we've failed to remove anything. */
  if (node->mIsEmpty) return false;

  /* Otherwise, if its min equals its max, then there's only one element 
   * left.
   */
  if (node->mMin == node->mMax) {
    /* If this doesn't match our element, we failed to remove it. */
    if (node->mMin != value) return false;

    /* Otherwise mark that this node is empty - we just removed the only
     * element from it.
     */
    node->mIsEmpty = true;
    return true;
  }

  /* If the value we're trying to erase is the min or max value, things get
   * tricky.  We need to replace the min or max with the min or max value of
   * the remaining elements.  These two cases are symmetric.
   */
  if (value == node->mMin) {
    /* Ask the summary for the tree of the smallest index that still has a
     * value; this is the smallest value in the summary.
     */
    size_t treeOffset = treeMin(node->mSummary, numBits / 2);

    /* If all the trees are empty, then we just copy over the maximum value
     * and are done.
     */
    if (treeOffset == kNil) {
      node->mMin = node->mMax;
      return true;
    }

    /* Otherwise, get the smallest value from the indicated tree, then remove
     * it from that tree.
     */
    size_t min = treeMin(node->mChildren[treeOffset], numBits / 2);
    recEraseElement(min, node->mChildren[treeOffset], numBits / 2);

    /* Now, if that tree ended up becoming empty, we need to remove the tree
     * offset from the summary structure.  Interestingly, we know that if the
     * subtree is now empty, the recursive call must have run in O(1), and so
     * at most one of these recursive calls will take any time to finish.
     */
    if (isTreeEmpty(node->mChildren[treeOffset], numBits / 2))
      recEraseElement(treeOffset, node->mSummary, numBits / 2);

    /* Finally, overwrite the minimum element with the minimum element of the
     * subtree.  We have to reconstitute the value from the offset and value.
     */
    node->mMin = Compose(treeOffset, min, numBits / 2);
    return true;
  }

  /* Similar logic for deleting the max. */
  if (value == node->mMax) {
    /* Ask the summary for the tree of the largest index that still has a
     * value; this is the max value in the summary.
     */
    size_t treeOffset = treeMax(node->mSummary, numBits / 2);

    /* If all the trees are empty, then we just copy over the minimum value
     * and are done.
     */
    if (treeOffset == kNil) {
      node->mMax = node->mMin;
      return true;
    }

    /* Otherwise, get the largest value from the indicated tree, then remove
     * it from that tree.
     */
    unsigned short max = treeMax(node->mChildren[treeOffset], numBits / 2);
    recEraseElement(max, node->mChildren[treeOffset], numBits / 2);

    /* Now, if that tree ended up becoming empty, we need to remove the tree
     * offset from the summary structure.  Interestingly, we know that if the
     * subtree is now empty, the recursive call must have run in O(1), and so
     * at most one of these recursive calls will take any time to finish.
     */
    if (isTreeEmpty(node->mChildren[treeOffset], numBits / 2))
      recEraseElement(treeOffset, node->mSummary, numBits / 2);

    /* Finally, overwrite the maximum element with the minimum element of the
     * subtree.  We have to reconstitute the value from the offset and value.
     */
    node->mMax = Compose(treeOffset, max, numBits / 2);
    return true;
  }

  /* Otherwise, the value isn't the max or min, so we just continue deleting
   * the value from the proper subtree.
   */
  const unsigned short treeOffset = UpperBits(value, numBits);
  bool result = recEraseElement(LowerBits(value, numBits),
                                node->mChildren[treeOffset],
                                numBits / 2);

  /* Check whether this emptied the tree.  If so, remove that tree from the
   * summary.
   */
  if (isTreeEmpty(node->mChildren[treeOffset], numBits / 2))
    recEraseElement(treeOffset, node->mSummary, numBits / 2);

  return result;
}

/* Querying for a successor just tries to bound what tree to search in. */
size_t VanEmdeBoasTree::recSuccessor(unsigned short value, void* root,
                                     size_t numBits) {
  /* If the tree is a bitvector, our search for a successor just involves
   * scanning the bits.
   */
  if (numBits <= kBitvectorSize) {
    const long bitvector = *static_cast<long*>(root);

    /* Starting right after the bit for this value, scan forward through the
     * bitvector for the first nonzero bit.
     */
    for (int index = value + 1; index < (1 << numBits); ++index)
      if (bitvector & (1 << index)) 
        return index;

    /* If we got here we didn't find anything, and so we should return the
     * sentinel value.
     */
    return kNil;
  }

  /* Otherwise, this must be a real node. */
  Node* node = static_cast<Node*>(root);

  /* If this tree is empty, the value has no successor. */
  if (node->mIsEmpty) 
    return kNil;

  /* If the value is less than the min, its successor is the min. */
  if (value < node->mMin)
    return node->mMin;

  /* If the value is at least as large as the max, it has no successor. */
  if (value >= node->mMax)
    return kNil;

  /* If neither of these cases hold, then the value is contained somewhere in
   * a subtree.  See what the largest value of that subtree is.
   */
  const unsigned short subtree = UpperBits(value, numBits);
  const size_t subtreeMax = treeMax(node->mChildren[subtree], numBits / 2);

  /* If that tree is empty or our value is at least as large as that value,
   * then the successor is given by the smallest value of the next available
   * tree.
   */
  if (subtreeMax == kNil || LowerBits(value, numBits) >= subtreeMax) {
    /* Ask the summary tree for the smallest tree beyond this value's subtree
     * that is nonempty.
     */
    size_t nextTree = recSuccessor(subtree, node->mSummary, numBits / 2);

    /* If there is no next tree, then the successor must be the tree's maximum
     * value.
     */
    if (nextTree == kNil)
      return node->mMax;

    /* Otherwise, it's the smallest value of that subtree.  Of course, we need
     * to take care to reconstitute the value we're returning.
     */
    return Compose(nextTree, treeMin(node->mChildren[nextTree], numBits / 2),
                   numBits / 2);
  }

  /* Otherwise, the subtree containing the value is correct, and so we can
   * descend into it to get the result.
   */
  return Compose(subtree, recSuccessor(LowerBits(value, numBits),
                                       node->mChildren[subtree],
                                       numBits / 2), numBits / 2);
}

/* Predecessor search is symmetric. */
size_t VanEmdeBoasTree::recPredecessor(unsigned short value, void* root,
                                       size_t numBits) {
  /* If the tree is a bitvector, our search for a successor just involves
   * scanning the bits.
   */
  if (numBits <= kBitvectorSize) {
    const long bitvector = *static_cast<long*>(root);

    /* Starting right before the bit for this value, scan forward through the
     * bitvector for the first nonzero bit.
     */
    for (int index = value - 1; index >= 0; --index)
      if (bitvector & (1 << index)) 
        return index;

    /* If we got here we didn't find anything, and so we should return the
     * sentinel value.
     */
    return kNil;
  }

  /* Otherwise, this must be a real node. */
  Node* node = static_cast<Node*>(root);

  /* If this tree is empty, the value has no predecessor. */
  if (node->mIsEmpty) 
    return kNil;

  /* If the value is above than the max, its predecessor is the max. */
  if (value > node->mMax)
    return node->mMax;

  /* If the value is no greater than the min, it has no predecessor. */
  if (value <= node->mMin)
    return kNil;

  /* If neither of these cases hold, then the value is contained somewhere in
   * a subtree.  See what the smallest value of that subtree is.
   */
  const unsigned short subtree = UpperBits(value, numBits);
  const size_t subtreeMin = treeMin(node->mChildren[subtree], numBits / 2);

  /* If that tree is empty or our value is no larger than that value, then 
   * the predecessor is given by the largest value of the smallest nonempty
   * tree before it.
   */
  if (subtreeMin == kNil || LowerBits(value, numBits) <= subtreeMin) {
    /* Ask the summary tree for the largest tree before this value's subtree
     * that is nonempty.
     */
    size_t nextTree = recPredecessor(subtree, node->mSummary, numBits / 2);

    /* If there is no next tree, then the predecessor must be the tree's 
     * minimum value.
     */
    if (nextTree == kNil)
      return node->mMin;

    /* Otherwise, it's the maximum value of that subtree.  Of course, we need
     * to take care to reconstitute the value we're returning.
     */
    return Compose(nextTree, treeMax(node->mChildren[nextTree], numBits / 2),
                   numBits / 2);
  }

  /* Otherwise, the subtree containing the value is correct, and so we can
   * descend into it to get the result.
   */
  return Compose(subtree, recPredecessor(LowerBits(value, numBits),
                                         node->mChildren[subtree],
                                         numBits / 2), numBits / 2);
}

/* Recursively cloning the tree involves cloning subtrees. */
void* VanEmdeBoasTree::recCloneTree(void* root, size_t numBits) {
  /* If we are using a bitvector, we need to copy the long. */
  if (numBits <= kBitvectorSize)
    return new long(*static_cast<long*>(root));

  /* Otherwise this is a node. */
  Node* node = static_cast<Node*>(root);

  /* Compute how many pointers we'll need.  This is 2^(numBits / 2). */
  const size_t numPointers = 1 << (numBits / 2);

  /* Otherwise, allocate a node and fill the fields in. */
  Node* result = new (numPointers) Node;

  /* Copy over the min, max, and whether the node is empty. */
  result->mIsEmpty = node->mIsEmpty;
  result->mMax = node->mMax;
  result->mMin = node->mMin;

  /* Copy the summary tree. */
  result->mSummary = recCloneTree(node->mSummary, numBits / 2);

  /* Copy each subtree. */
  for (size_t i = 0; i < numPointers; ++i)
    result->mChildren[i] = recCloneTree(node->mChildren[i], numBits / 2);

  return result;
}
