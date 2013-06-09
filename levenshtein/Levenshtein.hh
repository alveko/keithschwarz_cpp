/***************************************************************************
 * File: Levenshtein.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A function that computes the Levenshtein distance between two sequences of
 * elements.  The Levenshtein distance, or "edit distance," is the minimum
 * number of edits required to transform one string into another string, 
 * assuming the only legal edits are insertions, deletions, and substitutions.
 * As an example (borrowed from Wikipedia), converting "kitten" into "sitting"
 * could be done by transforming:
 *
 *                      kitten -> sitten -> sittin -> sitting
 *
 * For an edit distance of three.
 *
 * One (naive) way of computing the Levenshtein distance between two strings
 * is to consider all possible sequences of edits in a breadth-first fashion,
 * terminating when the first string can be converted into the second.  This
 * approach has a branching factor of 3n (each character could be edited,
 * deleted, or have something prepended), and is entirely infeasible.
 *
 * A (still naive) but more elegant approach is to instead focus just on the
 * the first characters of each of the strings.  Looking at the first letters,
 * it's possible to match them either by:
 *
 * - Editing one of the two so that they match (or doing nothing if they
 *   already match)
 * - Deleting one of the two, hoping that the next character matches.
 * - Adding a character to one of the strings to try to get the two to match.
 *
 * Searching with this technique still requires exponential time (since the
 * branching factor at each step is a constant), but it's much more feasible
 * than the initial approach.
 *
 * A final approach, and one that is substantially better than the others,
 * is to implement the above recursion bottom-up using dynamic programming.
 * Most of the recursive subproblems encountered via these three options
 * overlap with one another, and by computing the results from the bottom-up
 * the algorithm can avoid needlessly duplicating work.  We begin by 
 * constructing an (m + 1) x (n + 1) grid (where m and n are the length of A
 * and B, respectively).  The meaning of the grid entry at position (i, j) is 
 * "what is the minimum edit distance between the strings formed by the first i
 * characters of A and the first j characters of B?"  We initialize this grid
 * by setting G(0, j) = j, G(i, 0) = i, since the shortest way to get from
 * the empty string to the first j characters of B is to add those characters
 * one at a time, while the shortest way to get from the first i characters of
 * B to the empty string is to delete all those characters.  From here, we can
 * compute the rest of the entries bottom-up as follows:
 *
 * For every i, 0 <= i < |A|
 *   For every j, 0 <= j < |B|
 *      if A[i] = B[j], then G(i, j) = G(i - 1, j - 1)    (I)
 *      otherwise:
 *           G(i, j) = 1 + min(G(i - 1, j), G(i - 1, j - 1), 
 *                             G(i, j - 1))               (II)
 *
 * The intution behind this algorithm is follows.  To determine the minimum
 * edit distance from the first i characters of A to the first j characters of 
 * B, we consider if those characters match.  If so, we don't need to edit
 * those characters, and the total edit distance is the edit distance for the
 * rest of the string.  This gives recurrence relation (I).  Otherwise, if the
 * two strings don't match, then we need to force the strings to agree.  We can
 * do this by either:
 *
 *  1. Changing the last two characters to be the same.
 *  2. Removing the first character from string A
 *  3. Adding a replacement character to the end of B.
 *
 * Each of these operations costs one, and whichever ends up giving the 
 * cheapest solution for the rest of the string is the optimal answer.  This
 * gives us the recurrence relation (II).
 */

#ifndef Levenshtein_Included
#define Levenshtein_Included

#include "Grid.hh"
#include <iterator>  // For distance
#include <algorithm> // For min

/**
 * Function: LevenshteinDistance(ForwardIterator1 begin1, ForwardIterator1 end1,
 *                               ForwardIterator2 begin2, ForwardIterator2 end2);
 * Usage: cout << LevenshteinDistance(str1.begin(), str1.end(),
 *                                    str2.begin(), str2.end());
 * ---------------------------------------------------------------------------
 * Given two sequences defined by [begin1, end1) and [begin2, end2), returns
 * the Levenshtein edit distance between those two sequences.
 */
template <typename ForwardIterator1, typename ForwardIterator2>
size_t LevenshteinDistance(ForwardIterator1 begin1, ForwardIterator1 end1,
                           ForwardIterator2 begin2, ForwardIterator2 end2) {
  /* Begin by computing the sizes of the two ranges. */
  const size_t m = size_t(std::distance(begin1, end1));
  const size_t n = size_t(std::distance(begin2, end2));

  /* Construct a grid of the edit distances of the strings' prefixes. */
  Grid<size_t> distances(m + 1, n + 1);

  /* Fill in the first row and column using the setup G(i, 0) = i, G(0, j) = j. */
  for (size_t i = 0; i < distances.numRows(); ++i)
    distances[i][0] = i;
  for (size_t j = 0; j < distances.numCols(); ++j)
    distances[0][j] = j;

  /* Next, fill in the rest of the entries recursively.  Due to the fact that
   * the input iterators are ForwardIterators and not RandomIterators, we need
   * to keep track of both the (row, col) index in considering and the
   * iterators to those particular positions.
   */
  size_t i = 1;
  for (ForwardIterator1 itr1 = begin1; itr1 != end1; ++itr1, ++i) {
    size_t j = 1;
    for (ForwardIterator2 itr2 = begin2; itr2 != end2; ++itr2, ++j) {
      /* Now, apply the recurrence.  If the two elements match, just copy over
       * the minimum value from the prefixes of the two strings.
       */
      if (*itr1 == *itr2) {
        distances[i][j] = distances[i - 1][j - 1];
      }
      /* Otherwise, we need to check all possible actions. */
      else {
        distances[i][j] = 1 + std::min(distances[i - 1][j],
                                       std::min(distances[i][j - 1], 
                                                distances[i - 1][j - 1]));
      }
    }
  }
  
  /* Finally, our result can be found by looking at the element in the table
   * corresponding to the full strings, which is at position (m, n).
   */
  return distances[m][n];
}

#endif
