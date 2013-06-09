/*****************************************************************
 * File: Mergesort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the Mergesort algorithm.  This algorithm
 * is implemented with a focus on readability and clarity rather
 * than speed.  Ideally, this should give you a better sense
 * for how Mergesort works, which you can then use as a starting
 * point for implementing a more optimized version of the algorithm.
 *
 * In case you are not familiar with Mergesort, the idea behind the
 * algorithm is as follows.  Given an input list, we wish to sort
 * the list from lowest to highest.  To do so, we split the list
 * in half, recursively sort each half, and then use a merge algorithm
 * to combine the two lists back together.  As a base case for
 * the recursion, a list with zero or one elements in it is trivially
 * sorted.
 *
 * The only tricky part of this algorithm is the merge step, which
 * takes the two sorted halves of the list and combines them together
 * into a single sorted list.  The idea behind this algorithm is
 * as follows.  Given the two lists, we look at the first element of
 * each, remove the smaller of the two, and place it as the first
 * element of the overall sorted sequence.  We then repeat this
 * process to find the second element, then the third, etc.  For
 * example, given the lists 1 2 5 8 and 3 4 7 9, the merge
 * step would work as follows:
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 * 1 2 5 8      3 4 7 9
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *   2 5 8      3 4 7 9          1
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *     5 8      3 4 7 9          1 2
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *     5 8        4 7 9          1 2 3
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *     5 8          7 9          1 2 3 4
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *       8          7 9          1 2 3 4 5
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *       8            9          1 2 3 4 5 7
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *                    9          1 2 3 4 5 7 8
 *
 * FIRST LIST   SECOND LIST      OUTPUT
 *                               1 2 3 4 5 7 8 9
 *
 * The merge step runs in time O(N), and so using the Master Theorem
 * Mergesort can be shown to run in O(N lg N), which is asymptotically
 * optimal for a comparison sort.
 *
 * Our implementation sorts elements stored in std::vector<T>s, since
 * it abstracts away from the complexity of the memory management for
 * allocating the scratch arrays.
 */

#ifndef Mergesort_Included
#define Mergesort_Included

#include <vector>
#include <functional>

/**
 * Function: Mergesort(vector<T>& elems);
 * Usage: Mergesort(myVector);
 * ---------------------------------------------------
 * Applies the Mergesort algorithm to sort an array in
 * ascending order.
 */
template <typename T> 
void Mergesort(std::vector<T>& elems);

/**
 * Function: Mergesort(vector<T>& elems, Comparator comp);
 * Usage: Mergesort(myVector, std::greater<int>());
 * ---------------------------------------------------
 * Applies the Mergesort algorithm to sort an array in
 * ascending order according to the specified
 * comparison object.  The comparator comp should take
 * in two objects of type T and return true if the first
 * argument is strictly less than the second.
 */
template <typename T, typename Comparator>
void Mergesort(std::vector<T>& elems, Comparator comp);

/* * * * * Implementation Below This Point * * * * */

/* We store all of the helper functions in this detail namespace to avoid cluttering
 * the default namespace with implementation details.
 */
namespace detail {
  /* Given vectors 'one' and 'two' sorted in ascending order according to comparator
   * comp, returns the sorted sequence formed by merging the two sequences.
   */
  template <typename T, typename Comparator>
  std::vector<T> Merge(const std::vector<T>& one, const std::vector<T>& two, Comparator comp) {
    /* We will maintain two indices into the sorted vectors corresponding to 
     * where the next unchosen element of each list is.  Whenever we pick
     * one of the elements from the list, we'll bump its corresponding index
     * up by one.
     */
    size_t onePos = 0, twoPos = 0;

    /* The resulting vector. */
    std::vector<T> result;

    /* For efficiency's sake, reserve space in the result vector to hold all of
     * the elements in the two vectors.  To be truly efficient, we should probably
     * take in as another parameter an existing vector to write to, but doing so
     * would complicate this implementation unnecessarily.
     */
    result.reserve(one.size() + two.size());

    /* The main loop of this algorithm continuously polls the first and second
     * list for the next value, putting the smaller of the two into the output
     * list.  This loop stops once one of the lists is completely exhausted
     * so that we don't try reading off the end of one of the lists.
     */
    while (onePos < one.size() && twoPos < two.size()) {
      /* If the first element of list one is less than the first element of
       * list two, put it into the output sequence.
       */
      if (comp(one[onePos], two[twoPos])) {
        result.push_back(one[onePos]);

        /* Also bump onePos since we just consumed the element at that
         * position.
         */
        ++onePos;
      }
      /* Otherwise, either the two are equal or the second element is smaller
       * than the first.  In either case, put the first element of the second
       * sequence into the result.
       */
      else {
        result.push_back(two[twoPos]);
        ++twoPos;
      }
    }

    /* At this point, one of the sequences has been exhausted.  We should
     * therefore put whatever is left of the other sequence into the
     * output sequence.  We do this by having two loops which consume the
     * rest of both sequences, putting the elements into the result.  Of these
     * two loops, only one will execute, although it isn't immediately
     * obvious from the code itself.
     */
    for (; onePos < one.size(); ++onePos)
      result.push_back(one[onePos]);
    for (; twoPos < two.size(); ++twoPos)
      result.push_back(two[twoPos]);

    /* We now have merged all of the elements together, so we can safely
     * return the resulting sequence.
     */
    return result;
  }
}

/* Implementation of Mergesort itself. */
template <typename T, typename Comparator>
void Mergesort(std::vector<T>& elems, Comparator comp) {
  /* If the sequence has fewer than two elements, it is trivially in sorted
   * order and we can return without any more processing.
   */
  if (elems.size() < 2)
    return;

  /* Break the list into a left and right sublist. */
  std::vector<T> left, right;
  
  /* The left half are elements [0, elems.size() / 2). */
  for (size_t i = 0; i < elems.size() / 2; ++i)
    left.push_back(elems[i]);

  /* The right half are the elements [elems.size() / 2, elems.size()). */
  for (size_t i = elems.size() / 2; i < elems.size(); ++i)
    right.push_back(elems[i]);

  /* Mergesort each half. */
  Mergesort(left, comp);
  Mergesort(right, comp);

  /* Merge the two halves together. */
  elems = detail::Merge(left, right, comp);
}

/* The Mergesort implementation that does not require a comparator is implemented
 * in terms of the Mergesort that does use a comparator by passing in std::less<T>.
 */
template <typename T>
void Mergesort(std::vector<T>& elems) {
  Mergesort(elems, std::less<T>());
}

#endif
