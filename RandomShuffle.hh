/**************************************************************************
 * File: RandomShuffle.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implemention of a function for randomly permuting the elements of a
 * sorted range.  The algorithm is an implementation of the Fisher-Yates
 * shuffle (also called the Knuth shuffle), which works by randomly selecting
 * an element from the the array and swapping it to the front, then recursively
 * repeating the process on the rest of the array.  It runs in linear time
 * and with only constant space.
 *
 * This implementation, by default, works using the system rand function,
 * which is not ideal if cryptographic randomness is desired.  Consequently,
 * like the STL algorithm random_shuffle, the algorithm allows for a custom
 * random generator to be passed into the function.
 */
#ifndef RandomShuffle_Included
#define RandomShuffle_Included

#include <algorithm> // For iter_swap
#include <cstdlib>   // For rand

/**
 * Function: RandomShuffle(RandomIterator begin, RandomIterator end);
 * ------------------------------------------------------------------------
 * Randomly permutes the elements in the range [begin, end), using the system
 * rand function as a source of randomness.
 */
template <typename RandomIterator>
void RandomShuffle(RandomIterator begin, RandomIterator end);

/**
 * Function: RandomShuffle(RandomIterator begin, RandomIterator end,
 *                         RandomGenerator rng);
 * ------------------------------------------------------------------------
 * Randomly permutes the elements in the range [begin, end), using the
 * provided callback as a source of randomness.  The generator should be
 * callable as a nullary function which produces a uniformly-distributed
 * random value over a range at least as large as the input range.
 */
template <typename RandomIterator, typename RandomGenerator>
void RandomShuffle(RandomIterator begin, RandomIterator end,
                   RandomGenerator rnd);

/* * * * * Implementation Below This Point * * * * */

/* Main implementation of the algorithm. */
template <typename RandomIterator, typename RandomGenerator>
void RandomShuffle(RandomIterator begin, RandomIterator end,
                   RandomGenerator rnd) {
  /* Iterate across the elements, picking a random element and swapping it
   * to the front at each step.
   */
  for (RandomIterator itr = begin; itr != end; ++itr)
    std::iter_swap(itr, itr + rnd() % (end - itr));
}

/* Default implementation just uses rand. */
template <typename RandomIterator>
void RandomShuffle(RandomIterator begin, RandomIterator end) {
  RandomShuffle(begin, end, std::rand);
}

#endif
