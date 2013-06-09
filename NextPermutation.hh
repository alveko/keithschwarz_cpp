/************************************************
 * File: NextPermutation.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of the STL next_permutation
 * algorithm.  I've always found this particular
 * algorithm fairly interesting, but most
 * implementations obscure the core logic.
 *
 * The next_permutation algorithm takes in two
 * parameters delineating a range, then rearranges
 * the elements in the range so that they hold the
 * next lexicographically greater permutation of
 * the elements.  If a permutation is found, the
 * function returns true.  Otherwise, it returns
 * false and sorts the sequence once more.  This
 * allows you to consider all possible permutations
 * of a set of data as follows:
 *
 * do {
 *   ...
 * } while (next_permutation(v.begin(), v.end());
 *
 * The actual algorithm for next_permutation is
 * mostly straightforward, but does have a few
 * tricks.  The main idea is to look at the back
 * of the sequence for the longest continuously
 * decreasing sequence.  For example, given the
 * sequence 0 3 4 2 1, the longest decreasing
 * subsequence ending at the end of the sequence
 * is 4 2 1.  You can equivalently think of 
 * this sequence as the longest continuously
 * increasing subsequence formed by starting
 * at the end of the sequence and working
 * backwards.
 *
 * Because this subsequence is decreasing, we
 * cannot rearrange its elements to get a
 * larger permutation.  Consequently, if we want
 * to rearrange the elements in the overall
 * sequence to get a lexicographically greater
 * permutation, we will need to look at the
 * element right before the subsequence, which
 * must be less than at least one element of the
 * subsequence (since we define the subsequence
 * as the longest decreasing subsequence).
 * Once we have this element, we can exchange it
 * with the smallest element greater than it in
 * the subsequence.  We now have a permutation
 * which is lexicographically greater than the
 * one we started with.  For example, in the
 * sequence 0 3 4 2 1, the subsequence is
 * 4 2 1, and the element before the sequence
 * is 3.  We thus swap the 3 with the smallest
 * element in the subsequence bigger than it,
 * the 4, to get 0 4 3 2 1.  Now, this sequence
 * is lexicographically greater than what we
 * started with, but it is not the smallest
 * permutation greater than the one we started
 * with because the sequence 3 2 1 at the end
 * is the maximum permutation of those elements.
 * We thus reverse that sequence, yielding
 * 0 4 1 2 3, which is indeed the smallest
 * permutation lexicographically greater than
 * the input.
 *
 * To summarize, the algorithm is as follows:
 *
 * 1. Find the longest continuous decreasing
 *    subsequence that ends at the end of the
 *    sequence.  Let the element directly
 *    before this sequence be the crossover.
 * 
 * 2. Find the first element in the decreasing
 *    subsequence greater than the crossover,
 *    then exchange the crossover and this
 *    element.
 *
 * 3. Reverse the modified subsequence.
 */

#ifndef NextPermutation_Included
#define NextPermutation_Included

#include <iterator>  // For reverse_iterator
#include <algorithm> // For iter_swap, upper_bound, reverse

/**
 * Function: sorted_until
 * Usage: iterator itr = sorted_until(begin, end);
 * -----------------------------------------------
 * Given a range of values, returns an iterator to
 * the first value in the range which is strictly
 * less than its predecessor.  If the range is
 * sorted, this function returns end.
 */
template <typename ForwardIterator>
ForwardIterator sorted_until(ForwardIterator begin, ForwardIterator end) {
  /* Check if the range is empty.  If so, it's trivially sorted and
   * so we should hand back end.
   */
  if (begin == end) return end;

  /* Get an iterator one past begin.  If there isn't an element there,
   * the range has one element and is again trivially sorted.
   */
  ForwardIterator next = begin;
  ++next;

  if (next == end) return end;

  /* Continue marching the iterators forward until either:
   * 1. *next < *begin.  Then the mismatch is at next
   *    and we can signal this.
   * 2. next == end.  Then the entire range must be sorted
   *    because we walked off the end trying to find a
   *    mismatch.
   */
  for (; next != end; ++next, ++begin) {
    if (*next < *begin)
      return next;
  }
  
  /* If we're here, we fell off the end. */
  return end;
}

/**
 * Function: NextPermutation
 * Usage: do ... while(NextPermutation(begin, end));
 * ----------------------------------------------------
 * Given an input sequence, rearranges the elements of
 * that sequence to yield the lexicographically next
 * permutation.  If no such permutation exists (i.e.
 * the sequence is the lexicographically greatest
 * sequence), the function sorts the sequence and then
 * returns false.  Otherwise, it returns true.
 */
template <typename BidirectionalIterator>
bool NextPermutation(BidirectionalIterator begin,
                      BidirectionalIterator end) {
  /* Handy typedef. */
  typedef std::reverse_iterator<BidirectionalIterator> ReverseIterator;

  /* If there are no elements in the range, the range is the only
   * permutation of its elements and so we should return that no other
   * permutation exists.
   */
  if (begin == end) return false;

  /* Convert the range [begin, end) into [rbegin, rend) using 
   * reverse_iterator.
   */
  ReverseIterator rbegin(end), rend(begin);

  /* Use sorted_until to find the crossover point. */
  ReverseIterator crossover = sorted_until(rbegin, rend);

  /* If the entire sequence is sorted, then there are no more permutations.
   * This permutation is thus the lexicographically greatest, and so we
   * should reverse it to return it to its original state.
   */
  if (crossover == rend) {
    std::reverse(begin, end);
    return false;
  }

  /* Otherwise, find the first element in the range [rbegin, rend)
   * that's bigger than the crossover element and exchange it with
   * the crossover.
   */
  std::iter_swap(crossover, std::upper_bound(rbegin, crossover, *crossover));
  
  /* Reverse the sorted sequence. */
  std::reverse(rbegin, crossover);

  /* We just came up with a new permutation, so don't stop here. */
  return true;
}

#endif
