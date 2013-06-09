/*************************************************************************
 * File: RadixSort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the radix sort algorithm, which sorts numbers in
 * ascending order by sorting one digit at a time.  In particular, the
 * algorithm first sorts values by their least-signficant bit, then their
 * second-least significant bit, etc. until all of the numbers are in
 * sorted order.
 *
 * This particular implementation works on unsigned integers and sorts
 * them one bit at a time.  It maintains two queues, one of integers
 * with a 0 in the current bit position and one for integers with a 1
 * in the current bit position, then scans over the main list of values
 * putting each number in the proper queue.  It then dequeues the zero
 * queue, putting each element back into the array, then does the same
 * on the one queue.  Since this process preserves the relative ordering
 * of elements, at each phase the elements are always in sorted order
 * if one considers only the bits that have been visited so far.
 *
 * This implementation also allows for sorting of signed integers.  This
 * works by simply reversing the two queues when looking at the sign bit.
 */

#ifndef RadixSort_Included
#define RadixSort_Included

#include <climits> // For CHAR_BIT
#include <vector>
#include <iterator>
#include <algorithm>
#include <limits>

/**
 * Function: RadixSort(RandomIterator begin, RandomIterator end);
 * Usage: RadixSort(v.begin(), v.end());
 * ------------------------------------------------------------------------
 * Applies the radix sort algorithm to sort the specified list of numbers.
 * It is assumes that the iterators are traversing a list of integral
 * types, and will not function properly otherwise.
 */
template <typename RandomIterator>
void RadixSort(RandomIterator begin, RandomIterator end) {
  /* Typedef defining the type of the elements being traversed. */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;
  
  /* Two lists of values, one for zeros and one for ones. */
  std::vector<T> bitQueues[2];
  
  /* Traverse each bit from low to high until everything is sorted. */
  for (size_t bit = 0; bit < CHAR_BIT * sizeof(T); ++bit) {
    /* Traverse each element in the range, putting it into the proper 
     * queue.
     */
    for (RandomIterator itr = begin; itr != end; ++itr) {
      /* This next line is a bit cryptic.  There are three main steps.
       * 1. Compute a bitmask to read only one bit of the number.  This
       *    is given by 1u << bit.  The u here guarantees that the result
       *    is unsigned.
       * 2. Bitwise AND this with the current element.  This yields a large
       *    positive value if the bit is set, zero otherwise.
       * 3. Based on whether this value is nonzero, add it either to the
       *    first queue or the second queue.
       */
      bitQueues[(*itr & (1u << bit))? 1 : 0].push_back(*itr);
    }

    /* If this is the final bit and the representation of numbers is signed, then
     * this is the sign bit.  Consequently, we want to reverse the order of the
     * comparison.
     */
    if (std::numeric_limits<T>::is_signed && bit == CHAR_BIT * sizeof(T) - 1)
      bitQueues[0].swap(bitQueues[1]);
    
    /* Return the contents of the vectors to the array, with the zero
     * queue coming before the one queue.
     */
    std::copy(bitQueues[0].begin(), bitQueues[0].end(), begin);
    
    /* Output location starts after all of the elements from the zero queue. */
    std::copy(bitQueues[1].begin(), bitQueues[1].end(), begin + bitQueues[0].size());
    
    /* Clear the two queues. */
    bitQueues[0].clear();
    bitQueues[1].clear();
  }
}

#endif
