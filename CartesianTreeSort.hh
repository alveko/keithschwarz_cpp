/*****************************************************************************
 * File: CartesianTreeSort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a sort-style STL algorithm that uses a Cartesian tree
 * sort.  Cartesian tree sort is an adaptive, out-of-place sorting algorithm
 * with O(n) best-case behavior, O(n lg n) worst-case behavior, and O(n)
 * memory usage.
 *
 * A Cartesian tree is a tree created from a set of data that obeys the
 * following structural invariants:
 *
 * 1. The tree obeys in the min (or max) heap property - each node is less (or
 *    greater) than its children.
 * 2. An inorder traversal of the nodes yields the values in the same order in
 *    which they appear in the initial sequence.
 *
 * It's easy to see that this tree is unique by a quick induction on the size
 * of the input sequence.  As a base case, if the input sequence is empty,
 * then the empty tree is the unique Cartesian tree over that sequence.  For
 * the inductive case, assume that for all trees containing n' < n elements,
 * there is a unique Cartesian tree for each sequence of n' nodes.  Now take
 * any sequence of n elements.  Because a Cartesian tree is a min-heap, the
 * smallest element of the sequence must be the root of the Cartesian tree.
 * Because an inorder traversal of the elements must yield the input sequence,
 * we know that all nodes to the left of the min element must be in its left
 * subtree and similarly for the nodes to the right.  Since the left and right
 * subtree are both Cartesian trees with at most n - 1 elements in them (since
 * the min element is at the root), by the induction hypothesis there is a
 * unique Cartesian tree that could be the left or right subtree.  Since all
 * our decisions were forced, we end up with a unique tree, completing the
 * induction.
 *
 * An interesting note is that Cartesian trees are not necessarily height-
 * balanced.  In particular, any sequence in sorted or reverse-sorted order
 * will have a Cartesian tree that degrades to a linked list.  For example:
 *
 *                                    1 2 3 4 5
 *
 * has the following Cartesian tree:
 *
 *                                1
 *                                 \
 *                                  2
 *                                   \
 *                                    3
 *                                     \
 *                                      4
 *                                       \
 *                                        5
 *
 * In general, these Cartesian trees have height O(n).
 *
 * Interestingly, it's possible to build a Cartesian tree from a sequence of
 * data in linear time.  The algorithm as follows.  Beginning with the empty
 * tree, scan across the sequence from the left to the right adding new nodes
 * as follows:
 *
 * 1. Position the node as the right child of the rightmost node.
 * 2. Scan upward from the node's parent up to the root of the tree until a
 *    node is found whose value is less than the current value.
 * 3. If such a node is found, set its right child to be the new node, and
 *    set the new node's left child to be the previous right child.
 * 4. If no such node is found, set the new child to be the root, and set the
 *    new node's left child to be the previous tree.
 *
 * At first, this algorithm might not seem to run in linear time.  After all,
 * if the tree can become so imbalanced that it has height O(n) and we're
 * doing O(n) insertions, then it seems like the runtime should be O(n^2).
 * This bound is correct, but it isn't tight.  In particular, we can show that
 * the amortized cost of any insert is O(1), giving the net operations a total
 * runtime of O(n).  Define the potential of the tree to be the number of
 * nodes in its right spine.  The actual cost of any insertion is O(k), where
 * k is the number of nodes we considered on the way up from the rightmost
 * node to the node's new parent.  However, after we find where the node
 * belonds, we change the tree by moving k - 1 nodes into the left subtree of
 * the newly-inserted node.  This means that the tree's potential decreases by
 * k - 1.  We then increase the number of nodes in the right spine by one by
 * adding the new node there.  This means that the change in potential is
 * 1 - (k - 1) = 2 - k, giving an amortized cost of k + 2 - k = 2 = O(1) as
 * requested.
 *
 * Once we have built a Cartesian tree from a range of elements, we can sort
 * that range efficiently using a modified version of heapsort.  Construct a
 * binary heap that holds nodes in Cartesian trees and initialize it to the
 * root of the Cartesian tree.  Then, until the heap is empty, continuously
 * dequeue an element from the heap, add its root element to the next spot in
 * the sorted sequence, and then add the node's children to the heap.  This
 * visits each node in the sequence once, never visits a node until all of
 * its parents in the heap are visited, and visits the exposed roots in sorted
 * order.  This guarantees that the nodes come back sorted.  To see why,
 * suppose for the sake of contradiction two nodes come back out of order.
 * Call these nodes A and B with A > B.  Since the Cartesian tree is a min-
 * heap, B must not be an ancestor of A.  Since A came back first, B must not
 * have been an exposed root, or it would have come out of the heap before A.
 * But since B is not exposed, one of its ancestors must still be in the heap,
 * and since its ancestor has a value no greater than B's it would have come
 * out of the heap before A, a contradiction.
 *
 * Let's now consider the runtime of this phase.  We know that we will be
 * making O(n) insertions and dequeues from the binary heap, so each operation
 * takes at most O(lg n) time.  This gives us a worst-case runtime of
 * O(n lg n), matching the runtime guarantee of heapsort.  However, this bound
 * may not be tight.  In particular, suppose that our Cartesian tree is the
 * degenerate linked list described above.  Then initially the queue will have
 * exactly one element in it, and every time we dequeue the node and add its
 * children we'll only add a singleton node to the queue.  This means that the
 * queue will always have exactly one element in it, and so all the heap
 * operations will take O(1) time for a net runtime of O(n).  In fact, in any
 * Cartesian tree where each node has one child, we'll get this runtime.
 *
 * The interesting part about this algorithm is that it's possible to
 * explicitly quantify how much faster than O(n lg n) the algorithm will run
 * by using a measure called oscillation.  For any element x in the input
 * sequence, define cross(x) to be the number of adjacent pairs of entries
 * (y, z) in the input sequence such that either y <= x <= z or y >= x >= z.
 * Then we define the oscillation of the input sequence (denoted k) as the
 * average of cross(x) over all entries in the sequence.  It can be shown that
 * the overall runtime of the algorithm is O(n lg k), where k is this measure
 * of oscillation.  An interesting detail is that if the input is broken down
 * into S different sorted subsequences, k = O(S).  Consequently, if the
 * number of sorted subsequences in the input sequence is small (say, O(1)),
 * the algorithm will run in o(n lg n) time.  This result is due to the paper
 * "Heapsort, Adapted for Presorted Files," which first introduced Cartesian
 * tree sort.  Because the first step of the algorithm (building up the
 * Cartesian tree) runs in O(n), the overall runtime of the algorithm is
 * O(n lg k), which is at best O(n) and at worst O(n lg n).
 */

#ifndef CartesianTreeSort_Included
#define CartesianTreeSort_Included

/**
 * void CartesianTreeSort(ForwardIterator begin, ForwardIterator end);
 * Usage: CartesianTreeSort(v.begin(), v.end());
 * ---------------------------------------------------------------------------
 * Sorts the range [begin, end) into ascending order according to the default
 * ordering using the Cartesian tree sort algorithm.
 */
template <typename ForwardIterator>
void CartesianTreeSort(ForwardIterator begin, ForwardIterator end);

/**
 * void CartesianTreeSort(ForwardIterator begin, ForwardIterator end,
 *                        Comparator comp);
 * Usage: CartesianTreeSort(v.begin(), v.end(), std::greater<int>());
 * ---------------------------------------------------------------------------
 * Sorts the range [begin, end) into ascending order according to specified
 * comparator using the Cartesian tree sort algorithm.
 */
template <typename ForwardIterator, typename Comparator>
void CartesianTreeSort(ForwardIterator begin, ForwardIterator end,
                       Comparator comp);

/* * * * * Implementation Below This Point * * * * */
#include <iterator>   // For iterator_traits
#include <functional> // For less
#include <memory>     // For auto_ptr
#include <stack>
#include <queue>
#include <vector>

namespace cartesiantreesort_detail {
  /* A utility struct representing a node in a Cartesian tree. */
  template <typename T> struct Node {
    const T value;      // The node's value
    Node* left, *right; // Pointers to the proper subtrees

    /* Constructor: Node(const T& value);
     * Usage: Node* node = new Node(value);
     * -----------------------------------------------------------------------
     * Constructs a new Node having the specified value and no children.
     */
    explicit Node(const T& value) : value(value) {
      /* Initially this node is isolated. */
      left = right = NULL;
    }

    /* Destructor: ~Node();
     * Usage: (implicit)
     * -----------------------------------------------------------------------
     * Deallocates the tree rooted at this Node.
     */
    ~Node() {
      delete left;
      delete right;
    }
  };

  /* Node<T>* MakeCartesianTree(InputIterator begin, InputIterator end,
   *                            Comparator comp);
   * Usage: Node<T>* tree = MakeCartesianTree(begin, end, comp);
   * -------------------------------------------------------------------------
   * Constructs and returns a Cartesian tree containing the specified values
   * and sorted as a min-heap with respect to the given comparator.  The
   * return type of this function is a bit messy because it has to introspect
   * on the iterator type to figure out what's being stored.
   */
  template <typename InputIterator, typename Comparator>
  Node<typename std::iterator_traits<InputIterator>::value_type>*
  MakeCartesianTree(InputIterator begin, InputIterator end,
                    Comparator comp) {
    /* For sanity's sake, typedef the type being iterated over. */
    typedef typename std::iterator_traits<InputIterator>::value_type T;

    /* Keep track of the root of the tree, which is initially NULL because the
     * tree is empty.
     */
    Node<T>* root = NULL;

    /* In addition to this, we'll maintain a stack of the nodes on the right
     * spine of the tree, in the order in which you would encounter them if
     * you marched upward from the rightmost node to the root.
     */
    std::stack< Node<T>* > rightSpine;

    /* To avoid edge cases later on, we'll add NULL to the right spine.  This
     * does make some sense mathematically, since if we walk from the
     * rightmost node to the root and upward we'd walk off the tree at some
     * point.
     */
    rightSpine.push(NULL);

    /* Scan across the elements, adding them one at a time. */
    for (; begin != end; ++begin) {
      /* Construct the new node to insert. */
      Node<T>* node = new Node<T>(*begin);

      /* Starting at the rightmost node, walk upward along the right spine
       * until we find a node that can serve as the parent.  Because the spine
       * is never empty (NULL will always be there), we don't need to worry
       * about an empty stack.
       */
      Node<T>* curr;
      for (curr = rightSpine.top(); curr != NULL; rightSpine.pop(), curr = rightSpine.top())
        if (comp(curr->value, node->value))
          break;

      /* At this point, there are two cases to consider.  First, this new node
       * might be the new minimum.  In that case, we make it the global tree
       * root, and to preserve the inorder walk requirement make the old tree
       * its right child.
       */
      if (curr == NULL) {
        node->left = root;
        root = node;
      }
      /* Otherwise, we need to pull the current node's right subtree so that
       * it's the left subtree of the current node, and then set the new node
       * as the right child of the current node.
       */
      else {
        node->left = curr->right;
        curr->right = node;
      }

      /* This new node is now on the right spine, so we'll add it to the stack
       * of nodes stored there.
       */
      rightSpine.push(node);
    }

    /* Hand back the resulting tree. */
    return root;
  }

  /* A utility comparator class that compares Node<T>*s by the reverse of
   * their comparison by some comparator.  The rationale is that we will use
   * this comparator in a priority_queue of Node<T>*s, and will need some way
   * to ensure that the nodes are compared so that the smallest elements come
   * back first.
   */
  template <typename T, typename Comparator>
  class NodeComparator {
  public:
    /* Constructor: NodeComparator(Comparator comp);
     * Usage: NodeComparator comp(rawComp);
     * -----------------------------------------------------------------------
     * Constructs a new NodeComparator that uses the specified comparator on
     * the values in Node<T>*s.
     */
    explicit NodeComparator(Comparator comp) : comp(comp) {
      // Handled in initializer list
    }

    /* Comparator: bool operator() (const Node<T>* lhs, const Node<T>* rhs) const;
     * Usage: comp(one, two);
     * -----------------------------------------------------------------------
     * Returns whether the first node compares at least as large as the second
     * node using the stored comparator.
     */
    bool operator() (const Node<T>* lhs, const Node<T>* rhs) const {
      /* Check if lhs >= rhs by seeing if lhs < rhs returns false. */
      return !comp(lhs->value, rhs->value);
    }

  private:
    Comparator comp; // The actual comparator to use
  };
}

/* Actual implementation of Cartesian tree sort, using a parameterized
 * comparator.
 */
template <typename ForwardIterator, typename Comparator>
void CartesianTreeSort(ForwardIterator begin, ForwardIterator end,
                       Comparator comp) {
  /* As an edge case, check if the input is empty.  This avoids a problem
   * later on in this function where we might try enqueueing a NULL tree node
   * into the queue.
   */
  if (begin == end) return;

  /* Grant access to our helper types and classes. */
  using namespace cartesiantreesort_detail;

  /* Again, for sanity's sake, typedef the type being iterated over. */
  typedef typename std::iterator_traits<ForwardIterator>::value_type T;
  
  /* A type representing a priority queue that compares the value fields of
   * Cartesian tree nodes.
   */
  typedef std::priority_queue<Node<T>*, std::vector<Node<T>*>,
                              NodeComparator<T, Comparator> > PQueue;

  /* Construct a priority queue, wrapping up the comparator provided by the
   * client.  Due to the Most Vexing Parse, we have to parenthesize the
   * argument so this isn't accidentally interpreted as a function declaration.
   */
  PQueue pq((NodeComparator<T, Comparator>(comp)));

  /* Obtain a Cartesian tree over the input.  We'll store the result in a 
   * const auto_ptr to ensure that
   *
   * 1. The memory is reclaimed when the function exits and the auto_ptr
   *    leaves scope.
   * 2. The memory isn't accidentally transferred elsewhere, because the
   *    auto_ptr is const.
   */
  const std::auto_ptr< Node<T> > tree(MakeCartesianTree(begin, end, comp));

  /* Initialize the priority queue to hold the Cartesian tree of the input. */
  pq.push(tree.get());

  /* Now, scan across the sequence, placing the smallest known value at the
   * next open position and updating the queue accordingly.
   */
  for (ForwardIterator itr = begin; itr != end; ++itr) {
    /* Grab the next node from the queue. */
    Node<T>* curr = pq.top(); pq.pop();

    /* Store its value back into the sequence. */
    *itr = curr->value;

    /* Add any non-NULL subtrees of the current tree back into the queue. */
    if (curr->left) pq.push(curr->left);
    if (curr->right) pq.push(curr->right);
  }
}

/* Non-comparator version implemented in terms of the comparator version. */
template <typename ForwardIterator>
void CartesianTreeSort(ForwardIterator begin, ForwardIterator end) {
  CartesianTreeSort(begin, end,
                    std::less<typename std::iterator_traits<ForwardIterator>::value_type>());
}

#endif
