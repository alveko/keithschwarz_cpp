/*************************************************************************
 * File: InterpolationSearch.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the interpolation search algorithm.  Interpolation
 * search is a search algorithm influenced by binary search that, for many
 * data sets, performs asymptotically better.  Both binary and interpolation
 * search require the input data to be sorted, and use the sortedness to
 * rule out regions of the input from consideration.  They work by choosing
 * an element at a random position, comparing it to the element in question,
 * then deciding whether to continue the search on the left or the right.
 * The key difference is that binary search always works by splitting the
 * input range perfectly in half, which guarantees a runtime of O(lg n).
 * Interpolation search works by assuming that the data is distributed
 * uniformly, then doing a linear interpolation between the endpoints to
 * guess where the element ought to be.  Assuming the data is distributed
 * uniformly, it can be shown that interpolation search runs in expected
 * O(lg lg n) time, exponentially faster than binary search.  The proof is
 * rather involved, but if you're curious it can be found at
 *
 * www.cs.technion.ac.il/~itai/publications/Algorithms/p550-perl.pdf
 * 
 * However, in the worst case, interpolation search can degenerate to running
 * in O(n) time.  Assume, for example, that the input range is the series
 * 2^0, 2^2, 2^4, ..., 2^2n and that we wish to locate 2^1.  Interpolation
 * search would begin by linearly interpolating between 2^0 and 2^2n,
 * yielding ~2^(2n-1), and then guessing that the correct value must lie
 * somewhere in the vicinity of 2^2n and 2^(2n - 1).  This rules out the
 * value 2^2n from consideration, then recurses on the subrange
 * 2^0, 2^2, ..., 2^(2n - 2).  This process then continues to remove the last
 * element from the range repeatedly until the input is exhausted in O(n)
 * time.
 *
 * In practice, data is seldom distributed like this, and so interpolation
 * search is a reasonable choice for a search function.
 */
#ifndef InterpolationSearch_Included
#define InterpolationSearch_Included

#include <iterator> // For iterator_traits

/**
 * Function: InterpolationSearch(RandomIterator begin, RandomIterator end,
 *                               Element elem);
 * ------------------------------------------------------------------------
 * Performs interpolation search on the sorted range [begin, end).  It is
 * assumed that this range consists of finite integral values and that the
 * input is sorted in ascending order.  Returns whether the element was
 * located.
 */
template <typename RandomIterator, typename Element>
bool InterpolationSearch(RandomIterator begin, RandomIterator end,
                                   Element elem) {
  /* Get a type holding the distance between iterators in the range. */
  typedef typename std::iterator_traits<RandomIterator>::difference_type diffT;

  /* Edge-case: If there is no input, the element can't exist. */
  if (begin == end) return false;
 
  /* Continue looping while the value could feasibly be in the range and
   * the iterators haven't crossed.
   */
  while (*begin <= elem && elem <= *(end - 1) && begin != end) {
    /* Interpolate between the endpoints to guess where the element should
     * lie.  This works by computing the range [min, max] of the elements,
     * then seeing what fraction of the way up elem is.
     */
    const double interpolation = (double(elem) - *begin) / (double(*(end - 1)) - double(*begin));
  
    /* Scale this position to an index by multiplying by the number of elements
     * in the range by the fraction up to search.
     */
    RandomIterator mid = begin + diffT(interpolation * (double(end - begin) - 1));

    /* Apply standard binary search logic at this point.  If we found the element,
     * we're done.
     */
    if (*mid == elem) return true;
    /* Otherwise, if the element is smaller than what we're looking for, look
     * to the right.
     */
    else if (*mid < elem) begin = mid + 1;
    /* Otherwise, look to the right. */
    else end = mid;
  }

  /* If we're here, we didn't find the element in question. */
  return false;
}

#endif
