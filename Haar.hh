/********************************************************************
 * File: Haar.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of functions to encode a collection of values
 * in the range [0, 1] as Haar basis wavelets.  The function takes
 * as input a set of values (which must always be a power of two
 * in number), then produces a representation of those points as a
 * sum of Haar wavelets.  A similar function exists to invert the
 * encoding.
 *
 * These functions can be used to do signal compression (by converting
 * to a Haar basis, then dropping the lowest-order terms).
 */

#ifndef Haar_Included
#define Haar_Included

#include <vector>
#include <algorithm> // For copy
#include <iterator>  // For iterator_traits

/**
 * Function: HaarTransform(RandomIterator begin, RandomIterator end,
 *                         RandomIterator out);
 * Usage: HaarTransform(v.begin(), v.end(), output.begin());
 * ---------------------------------------------------------------------------
 * Applies the one-dimensional Haar transform to the elements in the range
 * [begin, end), storing the result in the range beginning at out.  It is 
 * assumed that all elements in the range [begin, end) have values in the 
 * range [0, 1].
 *
 * Internally, all operations are buffered into a temporary vector, and so it
 * is permissible for the input and output ranges to be coincident.
 */
template <typename RandomIterator, typename OutputIterator>
OutputIterator HaarTransform(RandomIterator begin, RandomIterator end,
                             OutputIterator out);

/**
 * Function: HaarTransformInverse(RandomIterator begin, RandomIterator end,
 *                                RandomOutputIterator out);
 * Usage: HaarTransformInverse(v.begin(), v.end(), recovered.begin());
 * ---------------------------------------------------------------------
 * Computes the inverse Haar transform of a range of values [begin, end),
 * storing the result in the range beginning with out.  Internally, elements
 * will be buffered to a vector, so it is permissible for the input and output
 * ranges to be coincident.
 */
template <typename RandomIterator, typename OutputIterator>
OutputIterator HaarTransformInverse(RandomIterator begin, RandomIterator end,
                                    OutputIterator out);

/* * * * * Implementation Below This Point * * * * */
template <typename RandomIterator, typename OutputIterator>
OutputIterator HaarTransform(RandomIterator begin, RandomIterator end,
                             OutputIterator out) {

  /* Utility typedefs to make it easier to talk about the underlying type of
   * elements being iterated over and the distances between iterators.
   */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;
  typedef typename std::iterator_traits<RandomIterator>::difference_type diffT;

  /* Base case: If there are no elements to transform, we just copy over
   * the values from the input range to the output.
   */
  if (end - begin <= diffT(1))
    return std::copy(begin, end, out);
  
  /* To allow for overlapping input and output regions, we create two temporary
   * vectors here, one to hold the smooth components and one to hold the
   * detail components.  We'll write this back at the end of the function.  
   * Since each recursive call uses n/2 auxiliary space this way, the total 
   * space overhead is O(n).
   */
  std::vector<T> smooth;
  std::vector<T> detail;
  
  /* Fill the buffer in by computing the smooth and detail components. */
  for (diffT i = 0; i < (end - begin) / diffT(2); ++i) {
    /* Smooth component given by (A + B) / 2 */
    smooth.push_back((begin[2 * i] + begin[2 * i + 1]) / 2.0);

    /* Detail component given by (A - B + 1) / 2. */
    detail.push_back((begin[2 * i] - begin[2 * i + 1]) / 2.0 + T(0.5));
  }

  /* Next, recursively transform the smooth region with the Haar transform. */
  out = HaarTransform(smooth.begin(), smooth.end(), out);

  /* Finally, write back the detail region. */
  return std::copy(detail.begin(), detail.end(), out);
}

template <typename RandomIterator, typename OutputIterator>
OutputIterator HaarTransformInverse(RandomIterator begin, RandomIterator end,
                                    OutputIterator out) {
  /* Utility typedefs to make it easier to talk about the underlying type of
   * elements being iterated over and the distances between iterators.
   */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;
  typedef typename std::iterator_traits<RandomIterator>::difference_type diffT;

  /* If the input range is empty. */ 
  if (begin == end)
    return out;

  /* Maintain a work buffer where the elements that have been expanded out
   * so far can live.  We will actually allocate two buffers here, since one
   * of them will be necessary to hold the elements and one will serve as
   * scratch space.  Each of them will be large enough to hold the entire
   * output range.
   */
  std::vector<T> workBuffer(end - begin), outputBuffer(end - begin);

  /* Initially, copy over the first element from the input range to the
   * work buffer.
   */
  workBuffer[0] = *begin;
  
  /* Continuously iterate over a larger and larger prefix, inverting the
   * transform.  Each iteration doubles the length of the prefix.
   */
  for (diffT k = 1; k < end - begin; k *= 2) {
    /* Iterate across the first k elements and invert the transformation
     * to the output range.
     */
    for (diffT i = 0; i < k; ++i) {
      /* The S value is what's currently stored in the work buffer, and its
       * matching D value can be found in the input range at position i + k.
       *
       * The formula for inversion is
       *
       * A = S + (D - 0.5);
       * B = S - (D - 0.5);
       */
      outputBuffer[2 * i]     = workBuffer[i] + (begin[i + k] - T(0.5));
      outputBuffer[2 * i + 1] = workBuffer[i] - (begin[i + k] - T(0.5));
    }

    /* After this process completes, exchange the output buffer and the
     * work buffer.  Now, the work buffer holds the prefix of S values
     * we've unpacked so far.
     */
    workBuffer.swap(outputBuffer);
  }

  /* Finally, once we've expanded everything out, the work buffer holds the
   * completely untransformed sequence.  We can then copy it to its final
   * destination.
   */
  return std::copy(workBuffer.begin(), workBuffer.end(), out);
}

#endif
