/*******************************************************************
 * File: Selection.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the linear-time order selection algorithm
 * (median of medians) invented by Blum, Floyd, Pratt, Rivest,
 * and Tarjan.  This classic algorithm takes as input an array
 * and an index, then repositions the elements in the array so
 * that the nth smallest element is in the correct index, all
 * smaller elements are to the left, and all larger elements are
 * to the right.
 *
 * The basic idea behind the algorithm is to repeatedly use the
 * partition algorithm to split the input into three ranges.
 * Partitioning works by picking an arbitrary element of the
 * range, then rearranging the elements of the array so that
 * the element is in its correct position in sorted order, the
 * elements to the left of the element are no bigger than the
 * element, and the elements to the right are no larger than
 * the element.  For example:
 *
 * -------------0+++++++++++++++++++++++++++
 *      ^       ^             ^
 *      |       |             |
 *      |       |             +--- Bigger elements
 *      |       +----------------- The element itself
 *      +------------------------- Smaller elements
 *
 * Once we have done this step, there are three cases.  First,
 * if the pivot element is now in the index we're looking for,
 * we can just return it.  Otherwise, if the element is to the
 * left, we recursively search the right side.  Finally, if the
 * element is to the right, we recursively search the right
 * side.
 *
 * This basic approach works marvelously well provided that every
 * time we perform a partition, we split the elements into roughly
 * even groups.  If this happens, we'll end up throwing away a
 * large amount of the input at each step and we get a nice linear
 * runtime.  However, we can run into trouble if our choice of
 * pivots for the partition step are bad and generate lopsided
 * splits.  This is akin to the behavior of quicksort - given
 * bad pivots, even a normally fast algorithm can degenerate to
 * quadratic behavior.
 *
 * To avoid this sort of problem, the median-of-medians algorithm
 * uses some clever recursion to ensure a good pivot choice.  In
 * particular, it works by breaking the input up into groups of
 * five elements, sorting each of those blocks, then taking their
 * medians.  It then recursively invokes the algorithm on those
 * medians to choose the median of those medians, then uses that
 * median as the pivot.  It can be shown that this choice of
 * pivot ends up splitting the input no worse than 70%/30%.
 * Using the formal definition of big-O, this can be shown to 
 * guarantee O(n) runtime in the worst case.
 *
 * The STL provides a variant of this algorithm called
 * nth_element which uses the partitioning scheme without the
 * clever choice of median.  On expectation it runs in O(n),
 * but can degenerate to O(n^2) on bad choices of pivot.
 */
#ifndef Selection_Included
#define Selection_Included

#include <functional>
#include <iterator>
#include <algorithm>

/**
 * Function: void Selection(RandomIterator begin, RandomIterator middle, 
 *                          RandomIterator end);
 * Usage: Selection(v.begin(), v.begin() + 5, v.end());
 * ----------------------------------------------------------------                     
 * Rearranges the elements of the range such that the element
 * at position middle is the element that would be there if the
 * elements were sorted, the elements [begin, middle) are all
 * no greater than middle, and the elements [middle + 1, end)
 * are all no less than middle.
 */
template <typename RandomIterator>
void Selection(RandomIterator begin, RandomIterator middle, RandomIterator end);

/**
 * Function: void Selection(RandomIterator begin, RandomIterator middle, 
 *                          RandomIterator end, Comparator comp);
 * Usage: Selection(v.begin(), v.begin() + 5, v.end());
 * ----------------------------------------------------------------                     
 * Rearranges the elements of the range such that the element
 * at position middle is the element that would be there if the
 * elements were sorted, the elements [begin, middle) are all
 * no greater than middle, and the elements [middle + 1, end)
 * are all no less than middle.  Comparisons are done according
 * to comp, which should be a strict weak ordering.
 */
template <typename RandomIterator, typename Comparator>
void Selection(RandomIterator begin, RandomIterator middle, RandomIterator end, 
               Comparator comp);

/* * * * * Implementation Detail Below This Point * * * * */
namespace selection_detail {
  /**
   * Function: Partition(RandomIterator begin, RandomIterator end, 
   *                     Comparator comp);
   * Usage: Partition(begin, end, comp);
   * -------------------------------------------------------------
   * Applies the partition algorithm to the range [begin, end),
   * assuming that the pivot element is pointed at by begin.
   * Comparisons are performed using comp.  Returns an iterator
   * to the final position of the pivot element.
   */
  template <typename RandomIterator, typename Comparator>
  RandomIterator Partition(RandomIterator begin, RandomIterator end,
                           Comparator comp) {
    /* To handle strange edge cases, check whether the range is 
     * empty or contains exactly one element.
     */
    if (begin == end || begin + 1 == end)
      return begin; // Pivot element, if it exists, is in the front.

    /* The following algorithm for doing an in-place partition is
     * one of the most efficient partitioning algorithms.  It works
     * by maintaining two pointers, one on the left-hand side of
     * the range and one on the right-hand side, and marching them
     * inward until each one encounters a mismatch.  Once the
     * mismatch is found, the mismatched elements are swapped and
     * the process continues.  When the two endpoints meet, we have
     * found the ultimate location of the pivot.
     *
     * This algorithm was originally developed by C.A.R. Hoare,
     * who also invented quicksort.
     */
    RandomIterator lhs = begin + 1;
    RandomIterator rhs = end - 1;
    while (true) {
      /* Keep marching the right-hand side inward until we encounter
       * an element that's too small to be on the left or we hit the
       * left-hand pointer.
       */
      while (lhs < rhs && !comp(*rhs, *begin))
        --rhs;
      /* Keep marching the left-hand side forward until we encounter
       * a the right-hand side or an element that's too big to be
       * on the left-hand side.
       */
      while (lhs < rhs && comp(*lhs, *begin))
        ++lhs;

      /* Now, if the two pointers have hit one another, we've found
       * the crossover point and are done.
       */
      if (lhs == rhs) break;

      /* Otherwise, exchange the elements pointed at by rhs and lhs. */
      std::iter_swap(lhs, rhs);
    }
    /* When we have reached this point, the two iterators have crossed
     * and we have the partition point.  However, there is one more edge
     * case to consider.  If the pivot element is the smallest element
     * in the range, then the two pointers will cross over on the first
     * step.  In this case, we don't want to exchange the pivot element
     * and the crossover point.
     */
    if (comp(*begin, *lhs))
      return begin;

    /* Otherwise, exchange the pivot and crossover, then return the
     * crossover.
     */
    std::iter_swap(begin, lhs);
    return lhs;
  }
}

/* Actual implementation of the Selection algorithm. */
template <typename RandomIterator, typename Comparator>
void Selection(RandomIterator begin, RandomIterator middle, RandomIterator end,
               Comparator comp) {
  /* Edge case checking - if there are no elements or exactly one element,
   * we're done.
   */
  if (begin == end || begin + 1 == end)
    return;

  /* The first step in this algorithm is to break the input up into groups of
   * five elements, sort each, and then recursively invoke the function to get
   * the median of those medians.  For simplicity, we'll do this by sorting
   * each block of five, then moving the median to the front of the list.
   * From there, we can recursively invoke Selection by just delineating the
   * front of the list.
   */
  RandomIterator nextMedianPosition = begin;

  /* Due to idiosyncrasies of the STL, while it's technically permissible to
   * walk an iterator an arbitrary number of steps off the end of a container,
   * it's not supported in most compilers.  Consequently, we'll compute the
   * position of the last block and iterate until we hit it.
   */
  RandomIterator lastBlockStart = begin + ((end - begin) / 5) * 5;
  for (RandomIterator blockStart = begin; blockStart != lastBlockStart; 
       blockStart += 5, ++nextMedianPosition) {
    /* Sort the block. */
    std::sort(blockStart, blockStart + 5, comp);

    /* Move the median to the next open median slot. */
    std::iter_swap(nextMedianPosition, blockStart + 2);
  }

  /* Now, if the last block isn't empty (i.e. lastBlockStart isn't
   * the end iterator), sort it as well and move the median to the
   * front.
   */
  if (lastBlockStart != end) {
    std::sort(lastBlockStart, end);
    std::iter_swap(nextMedianPosition, lastBlockStart + (end - lastBlockStart) / 2);

    /* Bump this value so we include it in the recursion. */
    ++nextMedianPosition;
  }

  /* Recursively invoke the function on the medians to get the median of medians. */
  RandomIterator medianOfMedians = begin + (nextMedianPosition - begin) / 2;
  Selection(begin, medianOfMedians, nextMedianPosition);

  /* Swap the new median down to the front and invoke partition to get the
   * crossover point.
   */
  std::iter_swap(medianOfMedians, begin);
  RandomIterator crossoverPoint = selection_detail::Partition(begin, end, comp);

  /* If the crossover point is the element we're looking for, that's great!
   * We're done.
   */
  if (crossoverPoint == middle) return;

  /* Otherwise, if the crossover point is to the right of the middle, recursively
   * invoke the function on the lower half.
   */
  else if (crossoverPoint > middle)
    Selection(begin, middle, crossoverPoint, comp);

  /* Otherwise, the crossover point is to the left.  Recursively invoke the function
   * on the upper half.
   */
  else
    Selection(crossoverPoint, middle, end, comp);
}

/* The version of this function without a comparator just uses std::less as a
 * comparator.
 */
template <typename RandomIterator>
void Selection(RandomIterator begin, RandomIterator middle, RandomIterator end) {
  /* The final parameter to this function is a bit tricky, but it means
   * "look up the type of the element being iterated over, then pass it
   * in as a parameter to the std::less comparator type."
   */
  return Selection(begin, middle, end,
                   std::less<typename std::iterator_traits<RandomIterator>::value_type>());
}

#endif
