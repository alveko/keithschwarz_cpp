/**
 * File: Kadane.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of Kadane's algorithm for solving the maximum subarray
 * problem.  In this problem, we are given an array A of real-valued entries 
 * and want to find the contiguous subarray of elements with maximum total sum.
 * This problem can be solved very naively in O(n^3) time by computing all
 * O(n^2) possible start and end points, then checking the sum of each range.
 * However, using dynamic programming, it's possible to optimize this to run in
 * O(n) time by looking more closely at the structure of an optimal solution.
 *
 * The idea behind Kadane's algorithm is to treat the problem recursively.  In
 * particular, we will consider the following question.  If we think about any
 * subarray, that subarray is defined by its start and endpoint.  If we 
 * consider the optimal solution to the problem, then it must still be optimal
 * if we were to truncate the array just after that subarray.  That is, there 
 * must be some index in the array for which the maximum-value subarray is the
 * largest subarray that ends at that particular index.  If we can scan across
 * the array and check which location this ends up being, and if we can check 
 * each possible location in O(1) time each, we'll have ourselves an O(n) 
 * algorithm for the problem.
 *
 * Fortunately, if we compute these values in a particular order, we can indeed
 * compute all of these values in time O(1) each.  The idea is as follows.
 * Let's define a function F(i) to be the weight of the maximum-weight subarray
 * ending just before position i in the array.  This means, for example, that
 * F(0) would be 0, since the weight of the maximum-weight subarray ending just
 * before the first character is the weight of the empty subarray, which is
 * zero.
 *
 * Now, let's think about the relationship between F(i) and F(i + 1).  That is,
 * if we were to take the maximum-weight subarray ending just before character
 * i, could we use information about it to determine the size of the maximum-
 * weight subarray ending just before element i + 1?  Well, let's think about
 * what the possible maximum subarrays ending just before element i + 1 might
 * look like.  We can take any number of elements from the suffix of the first
 * i elements of the array, of which the one that has the maximum value will be
 * given by F(i), the weight of the maximum-weight subarray ending at position
 * i.  Moreover, we know that F(i) >= 0, since in the worst case the largest
 * subarray ending at position i could just be the empty subarray.  Thus if we
 * want to consider the subarray formed by extending the maximum-weight
 * subarray ending just before position i, we would do so by adding in the 
 * array element at position i.  This means that one possible subarray we could
 * pick has value F(i) + A[i].  The other option is to take no elements at all,
 * which would happen if, for example, the value of A[i] is so enormously
 * negative that it would dwarf the result.  This means that we have the nice
 * recurrence that
 *
 *     F(0) = 0
 *     F(i + 1) = max { F(i) + A[i], 0 }
 *
 * This can be computed trivially by a forward scan over the array.  Once we
 * have all of the values of this function, the maximum one we encounter must
 * be the maximum-weight subarray for the overall array.  We can implement this
 * naively by actually storing all the values of F, but since all we care about
 * is the maximum value attained by F, we can do so in O(1) space by just
 * storing the maximum value encountered so far.  The result is an O(n)-time,
 * O(1)-space algorithm for finding the maximum-weight subarray of the array.
 * Moreover, the algorithm works perfectly fine on a stream of elements, since
 * we need just one pass over the data.
 */
#ifndef Kadane_Included
#define Kadane_Included

#include <iterator>  // For std::iterator_traits
#include <algorithm> // For std::max

/**
 * Function: MaximumSubarrayWeight(InputIterator begin, InputIterator end);
 * Usage: cout << MaximumSubarrayWeight(v.begin(), v.end());
 * ----------------------------------------------------------------------------
 * Given a range of elements [begin, end), returns the weight of the maximum-
 * weight subarray in the range [begin, end).
 */
template <typename InputIterator>
typename std::iterator_traits<InputIterator>::value_type
MaximumWeightSubarray(InputIterator begin, InputIterator end) {
  /* For sanity's sake, give the underlying type being iterated over a nicer
   * name.
   */
  typedef typename std::iterator_traits<InputIterator>::value_type ElemType;

  /* Initially, assume that the maximum value found so far is zero, since in
   * the worst-case we can just take nothing.
   */
  ElemType result(0);

  /* The value of the maximum-weight subarray ending just before the current
   * iterator, which corresponds to F(0) initially (with value zero).
   */
  ElemType endingValue(0);

  /* Scan across the elements, computing the maximum-weight subarray ending at
   * each point.  At every point, update our resulting maximum value based on
   * what we've found so far.
   */
  for (; begin != end; ++begin) {
    /* Update the value of the subarray ending at this element according to the
     * above recurrence relation.
     */
    endingValue = std::max(ElemType(0), endingValue + *begin);

    /* Update the global maximum value based on this candidate answer. */
    result = std::max(result, endingValue);
  }

  return result;
}

#endif
