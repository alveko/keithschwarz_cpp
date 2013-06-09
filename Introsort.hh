/***************************************************************************
 * File: Introsort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the introsort (introspective sort) algorithm, a
 * hybrid of quicksort, heapsort, and insertion sort that has particularly
 * good runtime behavior.  It is one of the fastest comparison sorting
 * algorithms in use today, and is the usual implementation of the std::sort
 * algorithm provided with the C++ STL.
 *
 * Introsort aims to get the benefits of quicksort (good locality, in-place,
 * fast runtime) without running into any of its degenerate cases.  To do so,
 * the algorithm begins by guessing what the appropriate depth for the
 * quicksort recursion should be, then fires off a quicksort routine.  If
 * quicksort ever makes too many recursive calls, the introsort routine
 * switches over to using heapsort to sort the range.  This means that in
 * the best case, the algorithm runs a standard quicksort with minimal
 * bookkeeping overhead and thus runs extremely quickly.  In the worst case,
 * the algorithm switches to heapsort and avoids the O(n^2) worst-case of
 * quicksort.
 *
 * The algorithm also contains an additional optimization.  Rather than
 * using the O(n lg n) sorts (quicksort and heapsort) to completely sort the
 * input, instead introsort picks some "block size" and then uses the sorts
 * only on subranges larger than the block size.  It then makes a final pass
 * over the input using insertion sort to fix up the range.  Since insertion
 * sort runs extremely quickly (O(n)) when all of the elements in the range
 * are known to be a constant number of positions from their final locations,
 * this step runs rapidly.  It also decreases the overall work necessary by
 * the algorithm, since heapsort and quicksort are expensive on small ranges.
 *
 * This implementation of introsort uses the provided STL implementation of
 * heapsort (make_heap, sort_heap) for simplicity, but has its own versions
 * of the quicksort and insertion sort routines.  It is based on David
 * Musser's original paper on introsort (which can be found at
 * http://www.cs.rpi.edu/~musser/gp/introsort.ps), though it does not use
 * directly any of the code it contains.
 */
#ifndef Introsort_Included
#define Introsort_Included

#include <algorithm>  // For iter_swap, make_heap, sort_heap
#include <functional> // For less
#include <iterator>   // For iterator_traits
#include <iostream>

/**
 * Function: Introsort(RandomIterator begin, RandomIterator end);
 * ------------------------------------------------------------------------
 * Sorts the range [begin, end) into ascending order using the introsort
 * algorithm.
 */
template <typename RandomIterator>
void Introsort(RandomIterator begin, RandomIterator end);

/**
 * Function: Introsort(RandomIterator begin, RandomIterator end,
 *                     Comparator comp);
 * -----------------------------------------------------------------------
 * Sorts the range [begin, end) into ascending order (according to comp)
 * using the introsort algorithm.
 */
template <typename RandomIterator, typename Comparator>
void Introsort(RandomIterator begin, RandomIterator end, Comparator comp);

/* * * * * Implementation Below This Point * * * * */
namespace introsort_detail {
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
    /* The following algorithm for doing an in-place partition is
     * one of the most efficient partitioning algorithms.  It works
     * by maintaining two pointers, one on the left-hand side of
     * the range and one on the right-hand side, and marching them
     * inward until each one encounters a mismatch.  Once the
     * mismatch is found, the mismatched elements are swapped and
     * the process continues.  When the two endpoints meet, we have
     * found the ultimate location of the pivot.
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

  /**
   * Function: MedianOfThree(RandomIterator one, RandomIterator two,
   *                         RandomIterator three, Comparator comp);
   * ---------------------------------------------------------------
   * Returns the middle element of the three, according to comp.
   */
  template <typename RandomIterator, typename Comparator>
  RandomIterator MedianOfThree(RandomIterator one, RandomIterator two,
                               RandomIterator three, Comparator comp) {
    /* Do all three comparisons to determine which is in the middle. */
    const bool comp12 = comp(*one, *two);
    const bool comp13 = comp(*one, *three);
    const bool comp23 = comp(*two, *three);

    /* Based on the relationships between them, return the proper entry. */
    if (comp12 && comp23) return two;               // 1  < 2  < 3
    if (comp12 && !comp23 && comp13) return three;  // 1  < 3 <= 2
    if (!comp12 && comp13) return one;              // 2 <= 1  < 3
    if (!comp12 && !comp13 && comp23) return three; // 2 <  3 <= 1
    if (comp12 && !comp13) return one;              // 3 <= 1  < 2
    return two;                                     // 3 <= 2 <= 1
  }

  /**
   * Function: IntrosortRec(RandomIterator begin, RandomIterator end,
   *                        size_t depth, Comparator comp);
   * ---------------------------------------------------------------------
   * Uses the introsort logic (hybridized quicksort and heapsort) to
   * sort the range [begin, end) into ascending order by comp.
   */
  template <typename RandomIterator, typename Comparator>
  void IntrosortRec(RandomIterator begin, RandomIterator end,
                    size_t depth, Comparator comp) {
    /* Constant controlling the minimum size of a range to sort.  Increasing
     * this value reduces the amount of recursion performed, but may increase
     * the final runtime by increasing the time it takes insertion sort to
     * fix up the sequence.
     */
    const size_t kBlockSize = 24;

    /* Cache how many elements there are. */
    const size_t numElems = size_t(end - begin);

    /* If there are fewer elements in the range than the block size, we're
     * done.
     */
    if (numElems < kBlockSize) return;

    /* If the depth is zero, sort everything using heapsort, then bail out. */
    if (depth == 0) {
      std::make_heap(begin, end, comp);
      std::sort_heap(begin, end, comp);
      return;
    }

    /* Otherwise, use a median-of-three to pick a (hopefully) good pivot,
     * and partition the input with it.
     */
    RandomIterator pivot = MedianOfThree(begin,                // First elem
                                         begin + numElems / 2, // Middle elem
                                         end - 1, comp);       // Last elem

    /* Swap the pivot in place. */
    std::iter_swap(pivot, begin);

    /* Get the partition point and sort both halves. */
    RandomIterator partitionPoint = Partition(begin, end, comp);
    IntrosortRec(begin, partitionPoint, depth - 1, comp);
    IntrosortRec(partitionPoint + 1, end, depth - 1, comp);
  }

  /**
   * Function: IntrosortDepth(RandomIterator begin, RandomIterator end);
   * ---------------------------------------------------------------------
   * Returns the maximum depth to which introsort should be run on a range
   * of the specified size.  This is currently 2 lg (|end - begin|), as
   * suggested in David Musser's paper.
   */
  template <typename RandomIterator>
  size_t IntrosortDepth(RandomIterator begin, RandomIterator end) {
    size_t numElems = size_t(end - begin);

    /* Compute lg(numElems) by shifting the number down until we zero it. */
    size_t lg2 = 0;
    for (; numElems != 0; numElems >>= 1, ++lg2)
      ;

    /* Return twice this value. */
    return lg2 * 2;
  }

  /**
   * Function: InsertionSort(RandomIterator begin, RandomIterator end,
   *                         Comparator comp);
   * ----------------------------------------------------------------------
   * Sorts the range [begin, end) into ascending order (according to comp)
   * using insertion sort.
   */
  template <typename RandomIterator, typename Comparator>
  void InsertionSort(RandomIterator begin, RandomIterator end,
                     Comparator comp) {
    /* Edge case check - if there are no elements or exactly one element,
     * we're done.
     */
    if (begin == end || begin + 1 == end) return;

    /* Starting at the second element and continuing rightward, put each
     * element in its proper position.
     */
    for (RandomIterator itr = begin + 1; itr != end; ++itr) {
      /* Continue swapping down until we hit the beginning or are in the
       * correct position.
       */
      for (RandomIterator test = itr; test != begin && comp(*test, *(test - 1)); --test)
        std::iter_swap(test, test - 1);
    }
  }
}

/* Implementation of introsort. */
template <typename RandomIterator, typename Comparator>
void Introsort(RandomIterator begin, RandomIterator end, Comparator comp) {
  /* Give easy access to the utiltiy functions. */
  using namespace introsort_detail;

  /* Fire off a recursive call to introsort using the depth estimate of
   * 2 lg (|end - begin|), as suggested in the original paper.
   */
  IntrosortRec(begin, end, IntrosortDepth(begin, end), comp);

  /* Use insertion sort to clean everything else up. */
  InsertionSort(begin, end, comp);
}

/* Non-comparator version calls the comparator version. */
template <typename RandomIterator>
void Introsort(RandomIterator begin, RandomIterator end) {
  Introsort(begin, end,
            std::less<typename std::iterator_traits<RandomIterator>::value_type>());
}

#endif
