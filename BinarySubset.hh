/******************************************************************************
 * File: BinarySubset.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A collection of functions for generating and manipulating subsets in
 * lexicographic order based on the bijective correspondence between binary 
 * numbers and subsets.  For a more detailed explanation of the algorithms
 * implemented here, refer to my writeup "Generating Subsets Lexicographically
 * with Binary Numbers and Cyclic Shifts," available online at
 *
 *                     www.keithschwarz.com/binary-subsets
 */
#ifndef BinarySubset_Included
#define BinarySubset_Included

/**
 * Function: NthSubset(ForwardIterator begin, ForwardIterator end,
 *                     OutputIterator result, Integer k);
 * Usage: NthSubset(v.begin(), v.end(), back_inserter(subset), 137u);
 * ----------------------------------------------------------------------------
 * Given the sorted range [begin, end) and an integer n, copies the nth
 * lexicographically-smallest subset of [begin, end) to the range starting at
 * result.  It is assumed that n is in range and that the integer type used to
 * hold n is large enough to hold 2^(end - begin).
 */
template <typename ForwardIterator, typename OutputIterator, typename Integer>
OutputIterator NthSubset(ForwardIterator begin, ForwardIterator end,
                         OutputIterator result, Integer k);

/**
 * Function: SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
 *                       ForwardIterator setBegin, ForwardIterator setEnd);
 * Usage: cout << SubsetIndex<size_t>(s.begin(), s.end(), v.begin(), v.end());
 * ----------------------------------------------------------------------------
 * Given a subset of [setBegin, setEnd) defined in [subsetBegin, subsetEnd),
 * returns the index of that subset in the sequence of lexicographically-
 * ordered subsets of the master set, assuming the elements of the master set
 * are stored in sorted order.  The resulting integral type is assumed to be
 * large enough to hold 2^(setEnd - setBegin)
 */
template <typename Integer, typename InputIterator, typename ForwardIterator>
Integer SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
                    ForwardIterator setBegin, ForwardIterator setEnd);

/**
 * Function: SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
 *                       ForwardIterator setBegin, ForwardIterator setEnd,
 *                       Comparator comp);
 * Usage: cout << SubsetIndex<size_t>(s.begin(), s.end(), v.begin(), v.end());
 * ----------------------------------------------------------------------------
 * Given a subset of [setBegin, setEnd) defined in [subsetBegin, subsetEnd),
 * returns the index of that subset in the sequence of lexicographically-
 * ordered subsets of the master set, assuming the elements of the master set
 * are stored in sorted order.  The resulting integral type is assumed to be
 * large enough to hold 2^(setEnd - setBegin).  Comparisons are done according
 * to the comparator comp.
 */
template <typename Integer, typename InputIterator, typename ForwardIterator,
          typename Comparator>
Integer SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
                    ForwardIterator setBegin, ForwardIterator setEnd,
                    Comparator comp);

/* * * * * Implementation Below This Point * * * * */
#include <iterator>   // For std::distance, std::iterator_traits
#include <functional> // For std::less
#include <cassert>

template <typename ForwardIterator, typename OutputIterator, typename Integer>
OutputIterator NthSubset(ForwardIterator begin, ForwardIterator end,
                         OutputIterator result, Integer k) {
  /* Begin by seeing how many elements are in the range [begin, end). */
  const Integer n(std::distance(begin, end));

  /* Now, we need to invert the set of shifts to get back the raw index of the
   * subset in the canonical ordering of binary numbers.  This works by looking
   * at the bits of the number and doing a reverse cyclic shift of the
   * appropriate size whenever we find a zero.
   *
   * Normally, I wouldn't write a loop that counts down an unsigned value to
   * zero, but it's okay here because we stop on the last bit, not after the
   * last bit.
   */
  for (Integer bitIndex(n); bitIndex > Integer(0); -- bitIndex) {
    /* See if the bit an index k is set.  If not, we need to flip everything
     * below this point.
     */
    if (((k >> bitIndex) & Integer(1)) == Integer(0)) {
      /* Do a cyclic shift backwards of the bits after the current bit.  We do
       * this by checking whether the bits after this one are identically zero.
       * If so, we overwrite them all with 1s.  Otherwise, we just subtract
       * one.
       *
       * To see what the remaining bits are, we compute the bitwise AND of
       * a list of ones that spans the bits after this one.  We get a list like
       * this by taking 2^k and subtracting one.
       */
      const Integer ones = (Integer(1) << bitIndex) - Integer(1);

      if (k & ones)
        -- k;
      else
        k |= ones;
    }
  }

  /* Now, we have converted k into a binary number from which we can read off
   * the elements of the set one at a time by picking elements where the bits
   * are zero and skipping elements where the bits are one.  For this loop,
   * because we do need to count all the way down to zero, we count up from 0
   * to n and flip the iteration counter to change [0, n) to (n, 0].
   *
   * On each iteration, we also increment the begin iterator so that we keep
   * track of "the current element."
   */
  for (Integer bitIndex(0); bitIndex < n; ++ bitIndex, ++ begin) {
    /* Flip the index around, avoiding underflow. */
    Integer realIndex = (n - Integer(1)) - bitIndex;

    /* See if that bit is set.  If not, pick the element. */
    if ((k & (Integer(1) << realIndex)) == Integer(0))
      *result++ = *begin;
  }

  return result;
}

/* To determine the index of a subset, we first build up the integer
 * corresponding to the bitmap of the subset, and then apply the appropriate
 * shifts to convert that number to its final position.
 */
template <typename Integer, typename InputIterator, typename ForwardIterator,
          typename Comparator>
Integer SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
                    ForwardIterator setBegin, ForwardIterator setEnd,
                    Comparator comp) {
  /* See how many elements there are in the range. */
  const Integer n(std::distance(setBegin, setEnd));

  /* Scan across the two ranges determining which elements are present in the
   * subset and which aren't.  As we're doing so, we'll set the appropriate
   * bits in the bitmask.
   *
   * For simplicity, we'll pessimistically assume that the set is empty and
   * that all the bits in the bitmap are one.  We'll then clear all the bits
   * where a match occurs.
   *
   * Since we have to count down and Integer may be unsigned, we'll invert the
   * loop counter.
   */
  Integer bitmap = (Integer(1) << n) - Integer(1);
  for (Integer bitIndex(0); setBegin != setEnd && subsetBegin != subsetEnd;
       ++ bitIndex) {
    /* See how the current elements of the sequences compare.  If the current
     * element of the sequences match, record a zero bit in the appropriate
     * index.
     */
    if (!comp(*setBegin, *subsetBegin) && !comp(*subsetBegin, *setBegin)) {
      const Integer realIndex = n - Integer(1) - bitIndex;
      bitmap &= ~(Integer(1) << realIndex);

      /* Advance both iterators forward, since we just consumed the element. */
      ++ setBegin;
      ++ subsetBegin;
    } else {
      /* We didn't consume the current element of the subset, so advance the
       * master set pointer forward so we can try again.
       */
      ++ setBegin;
    }
  }

  /* We should have consumed all the elements from the subset.  If not, then
   * the elements aren't drawn from the master set.
   */
  assert (subsetBegin == subsetEnd);

  /* Now, scan the bits from the least-to-most significant direction.  Whenever
   * we encounter a zero bit, perform a shift of the appropriate size.  Since a
   * shift of size one has no effect, as a microoptimization we'll start at
   * position one.  This also simplifies the logic for doing the shift.  Note
   * that we go one step past the end, because we need to use the zero bit
   * before the bitmask to do one final shift at the end.
   */
  for (Integer bitIndex(1); bitIndex <= n; ++ bitIndex) {
    /* If this bit is a zero, we need to cycle the rest of the bits. */
    if ((bitmap & (Integer(1) << bitIndex)) == Integer(0)) {
      /* We're doing a forward cycle of the rest of the bits, which maps any
       * bit pattern other than all ones to the current number plus one.  The
       * bit pattern of all ones gets mapped to all zeros.  We'll thus make an
       * integer with the appropriate pattern of ones and use a logical AND to
       * see which case we're in.
       */
      Integer ones = (Integer(1) << bitIndex) - Integer(1);
      if ((bitmap & ones) == ones)
        bitmap &= ~ones;
      else
        ++ bitmap;
    }
  }

  return bitmap;
}

/* Non-comparator SubsetIndex implemented in terms of comparator-based
 * SubsetIndex.
 */
template <typename Integer, typename InputIterator, typename ForwardIterator>
Integer SubsetIndex(InputIterator subsetBegin, InputIterator subsetEnd,
                    ForwardIterator setBegin, ForwardIterator setEnd) {
  return 
    SubsetIndex<Integer>(subsetBegin, subsetEnd, setBegin, setEnd,
                         std::less<typename std::iterator_traits<InputIterator>::value_type>());
}

#endif
