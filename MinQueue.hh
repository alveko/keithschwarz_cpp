/*****************************************************************************
 * File: MinQueue.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A FIFO queue class that supports amortized O(1) enqueue, dequeue, and find-
 * min.  Here, the find-min returns (but does not remove) the minimum value in
 * the queue.  This is not the same as a priority queue, which always removes
 * the smallest element on each dequeue.  The min-queue simply allows
 * for constant-time access to that element.
 *
 * The construction that enables the min-queue to work is based on a classic
 * construction for creating a queue out of two stacks.  We first discuss how
 * to do this, then explain how we generalize the construction to work for a
 * min-queue.
 *
 * The idea behind the queue-from-stacks construction is to maintain two
 * stacks, an "old" stack and a "new" stack.  Graphically, the stacks are
 * shown here:
 *
 *                  10      1
 *                   9      2
 *                   8      3
 *                   7      4
 *                   6      5
 *                  ---    ---
 *                  new    old
 *
 * Intuitively, the queue is represented by walking down the old stack from
 * top to bottom, then walking up the new stack from bottom to top.  In the
 * above picture, the queue contains the elements 1, 2, 3, 4, 5, 6, 7, 8, 9,
 * and 10, in that order.
 *
 * Whenever we enqueue a new element, we push it atop the new stack.  To
 * dequeue an element, if there are any elements in the old stack, we pop the
 * topmost element off.  Thus in the above picture, if we wanted to enqueue
 * the value 11, we'd end up with this setup:
 *
 *                  11
 *                  10      1
 *                   9      2
 *                   8      3
 *                   7      4
 *                   6      5
 *                  ---    ---
 *                  new    old
 *
 * Similarly, to dequeue the value 1, we'd pop it off of the old stack to get
 *
 *                  11
 *                  10      
 *                   9      2
 *                   8      3
 *                   7      4
 *                   6      5
 *                  ---    ---
 *                  new    old
 *
 * If we then dequeued 2, 3, 4, and 5, we would end up holding
 *
 *                  11
 *                  10      
 *                   9      
 *                   8     
 *                   7      
 *                   6      
 *                  ---    ---
 *                  new    old
 *
 * The question, now, is what happens if we want to dequeue another value from
 * the queue, given that the old stack is now empty.  To do this, we pop every
 * value from the new stack and push it into the old stack.  This reverses the
 * order of the elements in the new stack, giving
 *
 *                          6
 *                          7
 *                          8
 *                          9
 *                          10
 *                          11      
 *                  ---    ---
 *                  new    old
 *
 * From here, we can now dequeue the top value of the old stack to get 6, the
 * correct value.
 *
 * This algorithm may seem inefficient, since any dequeue operation might take
 * O(n) time to move over every element from the new array into the old.
 * However, in an amortized sense, the runtime of these operations is quite
 * fast.  We can show, using a proper analysis, that the amortized complexity
 * of any one operation is O(1).  To do this, we define a potential function
 * over the data structure such that P(q) is equal to the number of elements
 * in the 'new' stack.  From here, we get the following:
 *
 * 1. The actual complexity of enqueuing an element is 1 unit of work (pushing
 *    onto a stack), and it raises the potential by one.  This means that the
 *    amortized cost of the operation is 1 + DP(q) = 2 = O(1).
 * 2. The actual complexity of dequeuing an element depends on whether the old
 *    stack is empty or not:
 *
 *    *  If the old stack is empty, the actual complexity is 1 unit of work
 *       (popping off a stack) and there is no change in potential.  The
 *       amortized cost is thus 1 + DP(q) = 1 + 0 = O(1).
 *    *  If the old stack is not empty, the actual complexity is n units of
 *       work (popping n elements off one stack and moving them), plus one
 *       extra unit of work for the final pop for an actual complexity of
 *       n + 1 units of work.  However, this drops the potential by n, so the
 *       amortized complexity is n + 1 + DP(q) = n + 1 - n = 1 = O(1)
 *
 * Consequently, each operation takes amortized O(1) to complete.
 *
 * In order to use this solution to build a min-queue from two min-stacks, we
 * apply this construction to two min-stacks instead of two regular stacks.
 * The minimum element of the queue can then be found by looking at the
 * minimum element of the two stacks taken together.
 *
 * This implementation references the MinStack class, also available in the
 * Archive of Interesting Code.  You can find it online at
 *
 *          http://www.keithschwarz.com/interesting/code/?dir=min-stack
 */

#ifndef MinQueue_Included
#define MinQueue_Included

#include "MinStack.hh"
#include <functional>    // For std::less

/**
 * Class: MinQueue<T, Comparator = std::less<T> >
 * ---------------------------------------------------------------------------
 * A class representing a min-queue of elements of type T ordered according to
 * some Comparator type.
 */
template <typename T,
          typename Comparator = std::less<T> >
class MinQueue {
public:
  /**
   * Constructor: MinQueue(Comparator = Comparator());
   * Usage: MinQueue<T> myQueue;
   *        MinQueue<T, C> myQueue(myComparator);
   * -------------------------------------------------------------------------
   * Constructs a new MinQueue that uses the specified comparator to make
   * comparisons.
   */
  explicit MinQueue(Comparator = Comparator());

  /**
   * void enqueue(const T& val);
   * Usage: myQueue.enqueue(137);
   * -------------------------------------------------------------------------
   * Enqueues a new element into the min queue.
   */
  void enqueue(const T& val);

  /**
   * void dequeue();
   * Usage: myQueue.dequeue();
   * -------------------------------------------------------------------------
   * Dequeues the front of the queue.  If the queue is empty, the behavior is
   * undefined.
   */
  void dequeue();

  /**
   * const T& front() const;
   * Usage: cout << myQueue.front() << endl;
   * -------------------------------------------------------------------------
   * Returns an immutable view of the front element of the queue.  If the
   * queue is empty, the behavior is undefined.
   */
  const T& front() const;

  /**
   * const T& min() const;
   * Usage: cout << myQueue.min() << endl;
   * -------------------------------------------------------------------------
   * Returns an immutable view of the minimum element of the queue.  If the
   * queue is empty, the behavior is undefined.  If multiple elements in the
   * queue are tied for the minimum element, returns a reference to the lowest
   * (eldest) of them.
   */
  const T& min() const;

  /**
   * bool   empty() const;
   * size_t size()  const;
   * Usage: while (!myQueue.empty()) { ... }
   *        if (myQueue.size() == 3) { ... }
   * -------------------------------------------------------------------------
   * Returns whether the queue is empty and its size, respectively.
   */
  bool   empty() const;
  size_t size() const;
private:
  /* These stacks are marked mutable so that we can move elements across them
   * when we need to look at the top element.
   */
  mutable MinStack<T, Comparator> mNew, mOld;

  /* The comparator used to rank elements. */
  Comparator mComp;

  /* A utility function to move the contents of the new stack into the old
   * stack if the old stack is empty.
   */
  void moveIfNecessary() const;
};

/* * * * * Implementation Below This Point * * * * */

/* Constructor accepts a comparator and forwards it to both of the nested
 * stacks.
 */
template <typename T, typename Comparator>
MinQueue<T, Comparator>::MinQueue(Comparator c) 
  : mNew(c), mOld(c), mComp(c) {
  // Handled in initializer list
}

/* To retrieve the size of the queue and whether it's empty, we query the
 * underlying stacks.
 */
template <typename T, typename Comparator>
size_t MinQueue<T, Comparator>::size() const {
  return mNew.size() + mOld.size();
}
template <typename T, typename Comparator>
bool MinQueue<T, Comparator>::empty() const {
  return mNew.empty() && mOld.empty();
}

/* To retrieve the front element of the queue, we ensure that the old stack is
 * not empty, then look at its top element.
 */
template <typename T, typename Comparator>
const T& MinQueue<T, Comparator>::front() const {
  moveIfNecessary();
  return mOld.top();
}

/* To dequeue from the queue, we ensure that the old stack is not empty, then
 * remove its first element.
 */
template <typename T, typename Comparator>
void MinQueue<T, Comparator>::dequeue() {
  moveIfNecessary();
  mOld.pop();
}

/* To enqueue a new element, we just put it atop the new stack. */
template <typename T, typename Comparator>
void MinQueue<T, Comparator>::enqueue(const T& elem) {
  mNew.push(elem);
}

/* To find the minimum element, we may need to look at the elements of both
 * the stacks.
 */
template <typename T, typename Comparator>
const T& MinQueue<T, Comparator>::min() const {
  /* Case 1: If both queues are non-empty, compare them and return whichever
   * is smaller.
   */
  if (!mNew.empty() && !mOld.empty()) {
    /* Compare the two and return whichever is smaller. */
    return mComp(mOld.min(), mNew.min()) ? mOld.min() : mNew.min();
  }
  else if (!mNew.empty()) {
    /* Case two: Only the new queue is nonempty.  Returns its minimum. */
    return mNew.min();
  }
  else {
    /* Case three: Only the old queue is nonempty.  Return its minimum.  Note
     * that it's also possible that the whole queue is empty, which then leads
     * to undefined behavior.
     */
    return mOld.min();
  }
}

/* Logic to actually move the elements from the old stack to the new stack if
 * necessary.
 */
template <typename T, typename Comparator>
void MinQueue<T, Comparator>::moveIfNecessary() const {
  /* If the old stack isn't empty, don't move anything. */
  if (!mOld.empty()) return;

  /* While there are elements in the new stack, keep moving them over to the
   * old stack.
   */
  while (!mNew.empty()) {
    mOld.push(mNew.top());
    mNew.pop();
  }
}

#endif
