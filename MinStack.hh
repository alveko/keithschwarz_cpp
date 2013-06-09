/******************************************************************************
 * File: MinStack.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A LIFO stack class that supports O(1) push, pop, and find-min.  Here, the
 * find-min operation returns (but does not remove) the minimum value in the
 * stack.  This sort of stack is often used as a subroutine in other problems.
 * It can be used to construct a queue with equivalent properties by
 * using one of the many stack-to-queue constructions, for example.
 *
 * The implementation of the min-stack is actually quite simple.  The idea is
 * that since the stack can only grow and shrink at the top, we only need to
 * consider two ways that the minimum element of the stack can change:
 *
 *  1. The minimum element is on the top, and it is popped off, and
 *  2. A new element is added to the stack that is smaller than the minimum.
 *
 * Because of this, we can augment a standard stack to track the minimum value
 * as follows.  Whenever we push an element atop the stack, we compare it to
 * the current minimum value.  If it is smaller, we augment the value we just
 * added into the stack by recording that it is the minimum.  If not, we store
 * a pointer down to the element of the stack that actually is the minimum.  In
 * this way, we can find the minimum element in constant time by simply
 * inspecting the top element of the stack and following its pointer to the
 * minimum element.
 */
#ifndef MinStack_Included
#define MinStack_Included

#include <deque>
#include <functional> // For std::less
#include <utility>    // For std::pair

/**
 * Class: MinStack<T, Comparator = std::less<T>>
 * Usage: MinStack<int> myMinStack;
 * ----------------------------------------------------------------------------
 * A class representing a LIFO stack supporting constant-time push, pop, and
 * find-min.  The comparator may be customized.
 */
template <typename T, 
          typename Comparator = std::less<T> > 
class MinStack {
public:
  /**
   * Constructor: MinStack(Comparator = Comparator());
   * Usage: MinStack<T> myStack;
   *        MinStack<T, C> myStack(myComparator);
   * --------------------------------------------------------------------------
   * Constructs a new MinStack that uses the specified comparator to make
   * comparisons.
   */
  explicit MinStack(Comparator = Comparator());

  /**
   * void push(const T& val);
   * Usage: myStack.push(137);
   * --------------------------------------------------------------------------
   * Pushes a new element atop the stack.
   */
  void push(const T& val);

  /**
   * void pop();
   * Usage: myStack.pop();
   * --------------------------------------------------------------------------
   * Pops the top element off the stack.  If the stack is empty, the behavior
   * is undefined.
   */
  void pop();

  /**
   * const T& top() const;
   * Usage: cout << myStack.top() << endl;
   * --------------------------------------------------------------------------
   * Returns an immutable view of the top element of the stack.  If the stack
   * is empty, the behavior is undefined.
   */
  const T& top() const;

  /**
   * const T& min() const;
   * Usage: cout << myStack.min() << endl;
   * --------------------------------------------------------------------------
   * Returns an immutable view of the minimum element of the stack.  If the
   * stack is empty, the behavior is undefined.  If multiple elements in the
   * stack are tied for the minimum element, returns a reference to the lowest
   * (eldest) of them.
   */
  const T& min() const;

  /**
   * bool   empty() const;
   * size_t size()  const;
   * Usage: while (!myStack.empty()) { ... }
   *        if (myStack.size() == 3) { ... }
   * --------------------------------------------------------------------------
   * Returns whether the stack is empty and its size, respectively.
   */
  bool   empty() const;
  size_t size() const;

private:
  /* The actual stack.  Each entry is a pair of an element and the index of the
   * minimum element at or below this point.
   */
  std::deque< std::pair<T, size_t> > mStack;

  /* The comparator used to determine what the smallest element is. */
  Comparator mComp;
};

/* * * * * Implementation Below This Point * * * * */

/* Constructor stores the comparator for future use. */
template <typename T, typename Comparator>
MinStack<T, Comparator>::MinStack(Comparator c) 
  : mStack(), mComp(c) {
  // Handled in initialization list
}

/* Size and empty queries forward directly to the underlying deque. */
template <typename T, typename Comparator>
size_t MinStack<T, Comparator>::size() const {
  return mStack.size();
}
template <typename T, typename Comparator>
bool MinStack<T, Comparator>::empty() const {
  return mStack.empty();
}

/* Returning the top element looks at the back of the deque. */
template <typename T, typename Comparator>
const T& MinStack<T, Comparator>::top() const {
  return mStack.back().first;
}

/* Returning the min element looks at the element in the deque that is the
 * smallest so far.  It's held at the index at the top of the stack. */
template <typename T, typename Comparator>
const T& MinStack<T, Comparator>::min() const {
  return mStack[mStack.back().second].first;
}

/* Inserting a new element adds it to the stack and updates the minimum element
 * if the new element is smaller.
 */
template <typename T, typename Comparator>
void MinStack<T, Comparator>::push(const T& elem) {
  /* If the stack is empty, add the new element and mark that it's the smallest
   * so far.
   */
  if (empty()) {
    mStack.push_back(std::make_pair(elem, 0));
  }
  else {
    /* Otherwise, find the index of the smallest element and insert the new
     * element annotated with that index.
     */
    size_t smallestIndex = mStack.back().second;

    /* If this new element is smaller, the smallest element will now be at the
     * back of the list.
     */
    if (mComp(elem, min()))
      smallestIndex = mStack.size();

    /* Add the element in. */
    mStack.push_back(std::make_pair(elem, smallestIndex));
  }
}

/* Popping an element off the stack just removes the top pair from the
 * deque.
 */
template <typename T, typename Comparator>
void MinStack<T, Comparator>::pop() {
  mStack.pop_back();
}

#endif
