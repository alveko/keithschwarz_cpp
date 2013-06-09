/**
 * File: GeneralizedKadane.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A generalization of Kadane's algorithm for solving the maximum subarray
 * problem.  In the maximum subarray problem, we are given an array A of real-
 * valued entries and want to find the contiguous subarray of elements with 
 * maximum total sum.  In the generalized version of this problem, we are
 * restricted in what subarrays we can pick, and must choose a subarray whose
 * length is at least some number l (where l <= |A|, of course).  Before
 * reading this code, I would suggest reading over the original version of
 * Kadane's algorithm.  If you are interested, this code is available online
 * at the Archive of Interesting Code at
 *
 *             http://www.keithschwarz.com/interesting/code/?dir=kadane
 *
 * To recap, Kadane's algorithm is an optimized dynamic programming algorithm
 * that works by finding the maximum-weight subarray that ends just before each
 * index 0, 1, 2, ..., n in the array.  Since the maximum-weight subarray in
 * the global array must end somewhere, finding the maximum-weight subarray of
 * the arrays ending at these positions must find the overall best array.  To
 * find these values, Kadane's algorithm works by recognizing that the maximum-
 * weight subarray ending at some index i + 1 must be one of two arrays - it is
 * either the zero-element subarray ending just before position i + 1, or it is
 * the subarray formed by taking the maximum-weight subarray ending at position
 * i and extending it by one element.  Consequently, we can find the maximum-
 * weight subarray as follows:
 *
 *     Let best = 0 (the best subarray ending just before index 0 is empty)
 *     Current = 0  (the only subarray ending just before index 0 is empty)
 *     For i = 1 to n:
 *        current = max(0, current + arr[i - 1]);
 *        best = max(best, current)
 *
 * Here, the inner loop works by attempting to extend the subarray ending at
 * position i by the next element and seeing if this does any better than just
 * giving up and coming up with a new array.  After deciding which choice is
 * better, the algorithm updates the best value seen so far.
 *
 * If we want to generalize this to the case where the subarrays we consider
 * must have some minimum length L, then we need to update our approach.  In
 * particular, instead of keeping track of the value of the maximum-weight
 * subarray ending just before some position i, we should keep track of the
 * maximum-weight subarray *of length at least L* that ends just before some
 * position i.  Under this new interpretation, we can make the following
 * observations.  First, instead of looking before indices 0, 1, 2, ..., n, we
 * should instead look before indices L, L + 1, L + 2, ..., n, because there
 * are no subarrays of length at least L that end just before positions 0, 1,
 * 2, ..., L - 1.  Additionally, our DP recurrence no longer says that the
 * maximum-weight subarray of length at least L can be found by extending the
 * best subarray from the previous index by one or by resetting back to the
 * empty array, but rather by extending the previous subarray or resetting back
 * to the subarray of length L that comes right before the indicated index.
 *
 * If we use the iterative solution from Kadane's algorithm updated with this
 * change, we get an O(nL) algorithm that works as follows:
 *
 *    Compute the sum of the first L elements of the array.
 *    Set best and current to this value.
 *    For i = L + 1 to n:
 *       Set current = max(current + arr[i - 1], sum of arr[i - L] .. arr[i-1])
 *       Set best = max(best, current)
 *
 * Here, the O(nL) term comes from the fact that the inner loop executes O(n)
 * times, and on each iteration does O(L) work to compute the sum of the last L
 * array elements.
 *
 * However, we can optimize this down from O(nL) to O(n) using a clever trick.
 * Notice that in each loop iteration, we compute the sum of the last L array
 * elements.  For example, suppose that we set L = 4.  Then our loop will work
 * like this:
 *
 *     Before loop: Compute A[0] + A[1] + A[2] + A[3]
 *           i = 4: Compute        A[1] + A[2] + A[3] + A[4]
 *           i = 5: Compute               A[2] + A[3] + A[4] + A[5]
 *           i = 6: Compute                      A[3] + A[4] + A[5] + A[6]
 *
 * Notice that on each loop iteration, we are recomputing the sum of two terms
 * that had been computed on previous iterations.  In fact, more generally, if
 * we want to compute the sum of the past L array elements on each loop
 * iteration, we end up doing a sum of O(L) elements that we had previously
 * summed up on the previous loop iteration.  This is wasted work, and we can
 * optimize it away.  To do so, let us introduce some terminology.  Suppose 
 * that the initial sum of the first L elements is S.  Thus
 *
 *    S = A[0] + A[1] + ... + A[L - 1]
 *
 * Now, on the first loop iteration, we want to compute
 *
 *    A[1] + A[2] + ... + A[L]
 *
 * This value is given by
 *
 *     (A[0] + A[1] + ... + A[L - 1] + A[L]) - A[0]
 *   = S + A[L] - A[0]
 *
 * This technique generalizes to each iteration of the loop.  In fact, if S
 * is the sum of the L array elements ending just before position i, then the
 * value of the sum of the last L array elements ending before position i + 1
 * is given by S + A[i] - A[i - L].  Consequently, we can rewrite the inner
 * loop from above to work in only O(1) time per iteration by having it simply
 * do the O(1) work necessary to update the value of S from the previous loop
 * iteration.
 *
 * As a result, the algorithm's runtime is given as follows.  We begin by doing
 * O(L) = O(n) work to sum up the first L array elements.  We then iterate O(n)
 * times across the rest of the array elements, doing O(1) work on each step.
 * This gives an overall runtime of O(n), which is interesting because this
 * value is independent of the choice of L!
 */
#ifndef GeneralizedKadane_Included
#define GeneralizedKadane_Included

#include <iterator>  // For std::iterator_traits
#include <algorithm> // For std::max
#include <stdexcept> // For std::invalid_argument

/**
 * Function: GeneralizedMaximumSubarrayWeight(RandomAccessIterator begin, 
 *                                            RandomAccessIterator end,
 *                                            difference_type minLength)
 * Usage: cout << MaximumSubarrayWeight(v.begin(), v.end(), 4);
 * ----------------------------------------------------------------------------
 * Given a range of elements [begin, end), returns the weight of the maximum-
 * weight subarray in the range [begin, end) whose length is at least 
 * minLength.  If minLength is greater than the length of the range, an 
 * invalid_argument exception is thrown.
 */
template <typename RandomAccessIterator>
typename std::iterator_traits<RandomAccessIterator>::value_type
GeneralizedMaximumWeightSubarray(RandomAccessIterator begin, 
                                 RandomAccessIterator end, 
                                 typename std::iterator_traits<RandomAccessIterator>::difference_type minLength) {
  /* For sanity's sake, give the underlying type being iterated over and the
   * difference type nicer names.
   */
  typedef typename std::iterator_traits<RandomAccessIterator>::value_type ElemType;
  typedef typename std::iterator_traits<RandomAccessIterator>::difference_type DifferenceType;

  /* We do not need to confirm that minLength is nonnegative.  If it has a
   * negative value, then any subarray will work (since its length will be
   * at least minLength).
   */

  /* Track the sum of the last minLength array elements that we've seen.  We
   * need this in our recurrence as an option when considering whether to
   * extend the best subarray ending just before the last position or to toss
   * that array and just pick the best minLength elements before the array
   * index.
   */
  ElemType previousSum(0);
  for (DifferenceType i(0); i < minLength; ++i, ++begin) {
    /* If we can't find enough elements for the initial array, report an error;
     * we must have at least minLength elements to produce a sensible answer.
     */
    if (begin == end)
      throw std::invalid_argument("minLength longer than input sequence.");

    /* Add the current value into our total. */
    previousSum += *begin;
  }

  /* The value of the maximum-weight subarray ending just before the current
   * iterator, which corresponds to the sum of the first minLength elements
   * initially.
   */
  ElemType endingValue = previousSum;

  /* The best overall value so far is, initially, the sum of the first L array
   * elements.
   */
  ElemType result = previousSum;

  /* Scan across the elements, computing the maximum-weight subarray ending at
   * each point.  At every point, update our resulting maximum value based on
   * what we've found so far.
   */
  for (; begin != end; ++begin) {
    /* Update the sum of the last minLength elements by factoring in this new
     * element and subtracting out the element from several steps before.
     */
    previousSum += begin[0] - begin[-minLength];

    /* Update the value of the subarray ending at this element according to the
     * above recurrence relation.
     */
    endingValue = std::max(previousSum, endingValue + *begin);

    /* Update the global maximum value based on this candidate answer. */
    result = std::max(result, endingValue);
  }

  return result;
}

#endif
