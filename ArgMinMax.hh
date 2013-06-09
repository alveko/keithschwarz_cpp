/***************************************************************************
 * File: ArgMinMax.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of two algorithms, ArgMin and ArgMax, which take as inputs
 * a range of iterators and a function to call, then return iterators to the 
 * elements in the range for which the function obtains its minimum (maximum)
 * value.  These functions are similar to the STL min_element and max_element
 * functions, but are significantly easier to use when a simple function is 
 * being applied.  For example, compare:
 *
 *    double Haversine(double x) {
 *       return (1.0 - cos(x)) / 2.0;
 *    }
 *
 *    bool CompareByHaversines(double x, double y) {
 *       return Haversine(x) < Haversine(y);
 *    }
 *
 *    iterator itr = max_element(begin, end, CompareByHaversines);
 *
 * To
 *
 *    double Haversine(double x) {
 *       return (1.0 - cos(x)) / 2.0;
 *    }
 *
 *    iterator itr = ArgMax(begin, end, Haversine);
 */
#ifndef ArgMinMax_Included
#define ArgMinMax_Included

/**
 * ForwardIterator ArgMax(ForwardIterator begin, ForwardIterator end, 
 *                        UnaryFunction function)
 * Usage: ForwardIterator itr = ArgMax(begin, end, Haversine);
 * -------------------------------------------------------------------------
 * Given a range of iterators [begin, end), returns an iterator to an element
 * in the range for which function attains a maximum value.
 */
template <typename ForwardIterator, typename UnaryFunction>
ForwardIterator ArgMax(ForwardIterator begin, ForwardIterator end,
                       UnaryFunction function);

/**
 * ForwardIterator ArgMin(ForwardIterator begin, ForwardIterator end, 
 *                        UnaryFunction function)
 * Usage: ForwardIterator itr = ArgMin(begin, end, Haversine);
 * -------------------------------------------------------------------------
 * Given a range of iterators [begin, end), returns an iterator to an element
 * in the range for which function attains a minimum value.
 */
template <typename ForwardIterator, typename UnaryFunction>
ForwardIterator ArgMin(ForwardIterator begin, ForwardIterator end,
                       UnaryFunction function);

/* * * * * Implementation Below This Point * * * * */
template <typename ForwardIterator, typename UnaryFunction>
ForwardIterator ArgMax(ForwardIterator begin, ForwardIterator end,
                       UnaryFunction function) {
  /* Initially, our guess is that the first argument is the max element. */
  ForwardIterator result = begin;

  /* Scan across the values, updating our guess at each point. */
  for (; begin != end; ++begin)
    if (function(*result) < function(*begin))
      result = begin;

  return result;
}
template <typename ForwardIterator, typename UnaryFunction>
ForwardIterator ArgMin(ForwardIterator begin, ForwardIterator end,
                       UnaryFunction function) {
  /* Initially, our guess is that the first argument is the min element. */
  ForwardIterator result = begin;

  /* Scan across the values, updating our guess at each point. */
  for (; begin != end; ++begin)
    if (function(*begin) < function(*result))
      result = begin;

  return result;
}

#endif
