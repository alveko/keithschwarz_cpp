/**************************************************************************
 * File: NaturalMergesort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the natural mergesort algorithm.  Natural mergesort
 * is a bottom-up mergesort algorithm that works by implicitly splitting the
 * input up into a sequence of ascending subranges, then merging adjacent
 * subranges together until the entire input is sorted.  Since at each stage
 * the number of sorted subranges decreases by a factor of two, and there
 * are at most n sorted subranges in the initial input, there can be at most
 * O(lg n) rounds of merging.  Moreover, each merge of two sequences takes
 * at most O(n) time, since the maximum size of any two sequences to merge
 * is at most the size of the input array.  This gives a worst-case upper
 * bound of O(n lg n) runtime.  The merging is done out-of-place for
 * simplicity and thus the algorithm uses O(n) auxiliary storage space.
 *
 * However, this algorithm runs very quickly if the input is already sorted
 * to some initial extent.  In the best case, if the input is already
 * fully-sorted, the algorithm will terminate after running one pass over
 * the input, using only O(n) time.  In this sense, natural mergesort is
 * an adaptive sorting algorithm.
 */
#ifndef NaturalMergesort_Included
#define NaturalMergesort_Included

#include <iterator>   // For iterator_traits
#include <algorithm>  // For copy
#include <utility>    // For pair
#include <functional> // For less
#include <vector>

/**
 * Function: NaturalMergesort(ForwardIterator begin, ForwardIterator end);
 * ------------------------------------------------------------------------
 * Sorts the range specified by [begin, end) using the natural mergesort
 * algorithm.  Auxiliary storage space is placed into a temporary vector.
 */
template <typename ForwardIterator>
void NaturalMergesort(ForwardIterator begin, ForwardIterator end);

/**
 * Function: NaturalMergesort(ForwardIterator begin, ForwardIterator end,
 *                            Comparator comp);
 * ------------------------------------------------------------------------
 * Sorts the range specified by [begin, end) using the natural mergesort
 * algorithm.  Auxiliary storage space is placed into a temporary vector,
 * and the sequence is ordered by the strict weak ordering comp.
 */
template <typename ForwardIterator, typename Comparator>
void NaturalMergesort(ForwardIterator begin, ForwardIterator end,
                      Comparator comp);

/* * * * * Implementation Below This Point * * * * */
namespace naturalmergesort_detail {
  /**
   * Function: SortedRangeEnd(ForwardIterator begin, ForwardIterator end,
   *                          Comparator comp);
   * ---------------------------------------------------------------------
   * Returns an iterator to the end of the longest nondecreasing range
   * starting at begin in the range [begin, end), according to comparison
   * comp.  If the entire sequence is sorted, end is returned.
   */
  template <typename ForwardIterator, typename Comparator>
  ForwardIterator SortedRangeEnd(ForwardIterator begin, ForwardIterator end,
                                 Comparator comp) {
    /* Edge case - if the input is empty, we're done. */
    if (begin == end) return end;

    /* Get an iterator that's one step ahead of begin. */
    ForwardIterator next = begin; ++next;

    /* Keep marching these iterators forward until we find a mismatch or
     * hit the end.  A mismatch occurs when the element after the current
     * is strictly less than the current element.
     */
    for (; !comp(*next, *begin) && next != end; ++next, ++begin)
      ;

    /* The above loop stops either when next is the end of the sequence or
     * when next is the crossover point.  In either case, return next.
     */
    return next;
  }

  /**
   * Function: Merge(ForwardIterator begin, ForwardIterator mid,
   *                 ForwardIterator end, Comparator comp);
   * ---------------------------------------------------------------------
   * Merges the sorted ranges [begin, mid) and [mid, end) together into
   * one sorted range, using comp as the comparator.
   */
  template <typename ForwardIterator, typename Comparator>
  void Merge(ForwardIterator begin, ForwardIterator mid,
             ForwardIterator end, Comparator comp) {
    /* Determine the type of the element being iterated over. */
    typedef typename std::iterator_traits<ForwardIterator>::value_type T;

    /* Create a vector of Ts that will hold the merged sequence. */
    std::vector<T> merge;

    /* Continuously choose the smaller of the two to go in front until some
     * range is consumed.
     */
    ForwardIterator one = begin, two = mid;
    while (one != mid && two != end) {
      if (comp(*one, *two)) { // First sequence has smaller element
        merge.push_back(*one);
        ++one;
      } else { // Second sequence has smaller element.
        merge.push_back(*two);
        ++two;
      }
    }

    /* Once one of the sequences has been exhausted, one of two cases holds:
     *
     * 1. The first sequence was consumed.  In that case, the rest of the
     *    second sequence is valid and we can just copy the merged sequence
     *    in front of it.
     * 2. The second sequence was consumed.  In that case, we copy the rest
     *    of the first sequence into the merged sequence, then write the
     *    merged sequence back.
     */
    if (two == end)
      merge.insert(merge.end(), one, mid);

    std::copy(merge.begin(), merge.end(), begin);
  }
}

/* Main implementation of the algorithm. */
template <typename ForwardIterator, typename Comparator>
void NaturalMergesort(ForwardIterator begin, ForwardIterator end,
                      Comparator comp) {
  /* Make utility functions implicitly available. */
  using namespace naturalmergesort_detail;

  /* As an edge case, if the input range is empty, we're trivially done. */
  if (begin == end) return;

  /* Track whether the current iteration of the algorithm has made any
   * changes.  If it didn't, then we can finish early.
   */
  bool haveMerged;

  /* Continuously loop, merging together ranges until the input is fully
   * sorted.
   */
  do {
    /* We have not yet merged anything, so clear this flag. */
    haveMerged = false;

    /* Scan forward in the loop, looking for adjacent pairs of sorted ranges
     * and merging them.  
     */
    for (ForwardIterator itr = begin; itr != end; ) {
      /* See how far this range extends. */
      ForwardIterator rangeEnd = SortedRangeEnd(itr, end, comp);

      /* If we hit the end of the range, we're done with this iteration. */
      if (rangeEnd == end) break;

      /* See where the end of that range is. */
      ForwardIterator nextRangeEnd = SortedRangeEnd(rangeEnd, end, comp);

      /* Merge this range with the range after it. */
      Merge(itr, rangeEnd, nextRangeEnd, comp);
      
      /* Flag that we did at least one merge so we don't stop early. */
      haveMerged = true;

      /* Advance the iterator to the start of the next sequence, which is
       * directly after the end of the second sorted range.
       */
      itr = nextRangeEnd;
    }
  } while (haveMerged);
}

/* Non-comparator version of the function just uses the default comparator. */
template <typename ForwardIterator>
void NaturalMergesort(ForwardIterator begin, ForwardIterator end) {
  NaturalMergesort(begin, end,
                   std::less<typename std::iterator_traits<ForwardIterator>::value_type>());
}

#endif
