/****************************************************************************
 * File: NeedlemanWunsch.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the Needleman-Wunsch algorithm for optimal string
 * alignment.  The algorithm takes as input two strings, A and B, then 
 * computes the cost of an optimal alignment of the two strings formed by
 * inserting gaps at various points in the strings.  Gaps can be inserted
 * anywhere in the two strings, but all non-gap characters must align
 * perfectly with one another.  For example, to align TREE and THREE, we could
 * use any of the following alignments:
 *
 *                      T-REE   -T-REE-   -----TREE
 *                      THREE   T-HRE-E   THREE----
 *
 * Of these, the first is clearly the best, and the algorithm's goal is to
 * find and return it.
 *
 * The Needleman-Wunsch algorithm is interesting for a variety of reasons.
 * Historically, it was one of the first major biological algorithms to use
 * dynamic programming, and is often one of the first DP algortihms taught in
 * most algorithms classes.  It is also interesting because a naive
 * implementation takes O(mn) memory, while a more clever version can use
 * O(min{m, n}) memory (this version is implemented here).
 *
 * The idea behind the algorithm is, like the Levenshtein distance algorithm,
 * to consider the optimal way of matching the first characters of the
 * strings.  There are three possible options:
 *
 * 1. If the first two characters match, the cost is the cost of matching
 *    the rest of the string.
 * 2. Otherwise, we could match the first character of the first string with
 *    a gap, then match the remainder of that string with the second string.
 *    The cost is one plus the cost of that matching.
 * 3. Finally, we could match the first character of the second string with
 *    a gap, for a total cost of one plus the cost of matching the rest of the
 *    second string with the first string.
 *
 * If we let C(i, j) be the cost of matching the first m characters of the
 * first string with the first n characters of the second string, this
 * recurrence relation is formalized as
 *
 * C(0, j) = j     (The only way to match one string with an empty string is
 *                  to match the rest of the string with blanks)
 * C(i, 0) = i     (Same)
 * C(i, j) = min{ C(i - 1, j - 1)  (if A[i] == B[j], one-indexed),
 *                1 + C(i - 1, j),
 *                1 + C(i, j - 1) }
 *
 * Because this recurrence displays a large degree of overlapping subproblems,
 * it's a perfect candidate for a dynamic programming solution.  We initialize
 * a grid of size (m + 1) x (n + 1) to zero, initialize the base cases (for 
 * when i == 0), and then proceed upward and across filling it in.  This uses 
 * O(mn) memory and runs in O(mn) time.
 *
 * However, we can do much better than this.  Look at the effect of the
 * recurrence as a function of n.  Each term touched by the recurrence looks
 * only at lower values of m in the same row as n, or at the previous row of
 * n.  This means that we don't actually need to store the entire table, and
 * in fact only need access to the last row.  This uses memory O(n), which is
 * significantly better than before.
 *
 * But we can do even better than this!  Let m and n be the lengths of strings
 * A and B, respectively.  If m < n, then we can exchange strings A and B and
 * then use only O(m) memory.  This means that our memory usage is thus
 * O(min{m, n}), much better than what we started with.
 *
 * Some variants of this problem assign different weights to matchings of
 * various characters with blanks, or allows each character to match against
 * each other character for some penalty.  We don't consider this here, but
 * it's quite easy to adapt the algorithm to handle this.
 */

#ifndef NeedlemanWunsch_Included
#define NeedlemanWunsch_Included

#include <vector>
#include <algorithm> // For min
#include <iterator>  // For distance

/**
 * Function: NeedlemanWunschDistance(ForwardIterator1 begin1,
 *                                   ForwardIterator1 end1,
 *                                   ForwardIterator2 begin2,
 *                                   ForwardIterator2 end2);
 * Usage: cout << NeedlemanWunschDistance(str1.begin(), str1.end(),
 *                                        str2.begin(), str2.end());
 * ---------------------------------------------------------------------------
 * Given two sequences defined by [begin1, end1) and [begin2, end2), returns
 * the Needleman-Wunsch distance between those two sequences.
 */
template <typename ForwardIterator1, typename ForwardIterator2>
size_t NeedlemanWunschDistance(ForwardIterator1 begin1,
                               ForwardIterator1 end1,
                               ForwardIterator2 begin2,
                               ForwardIterator2 end2) {
  /* Begin by computing the sizes of the two ranges.  If we find that the
   * first range is smaller than the second range, exchange the two and
   * return that cost.
   */
  const size_t oneSize = size_t(std::distance(begin1, end1));
  const size_t twoSize = size_t(std::distance(begin2, end2));
  if (oneSize < twoSize)
    return NeedlemanWunschDistance(begin2, end2, begin1, end1);

  /* Construct a vector to hold the DP matching values, as well as a scratch
   * vector for use during each round.
   */
  std::vector<size_t> match(twoSize + 1), roundMatch(twoSize + 1);

  /* Base case: Cost of matching zero characters of the first string with some
   * number of characters of the second string is the number of characters in
   * the second string.
   */
  for (size_t i = 0; i < match.size(); ++i)
    match[i] = i;

  /* Inductive case: The cost of matching the first i characters of the first
   * string with the second string is defined above.
   */
  size_t i = 1;
  for (ForwardIterator1 itr1 = begin1; itr1 != end1; ++itr1, ++i) {
    /* The cost of matching the first i characters of the first string with
     * zero characters from the second string is i, since everything has to be
     * matched with a gap.
     */
    roundMatch[0] = i;

    /* Compute the recurrence. */
    size_t j = 1;
    for (ForwardIterator2 itr2 = begin2; itr2 != end2; ++itr2, ++j) {
      /* Compute the best we can do without applying a match. */
      size_t bestScore = 1 + std::min(roundMatch[j - 1], match[j]);

      /* If the characters match, update this to consider what happens when
       * we match them.
       */
      if (*itr1 == *itr2)
        bestScore = std::min(bestScore, match[j - 1]);

      /* Write the score out to the round vector. */
      roundMatch[j] = bestScore;
    }

    /* Update the resulting match score by swapping the scores from last round
     * and this round.  On the next round, we'll use the old match vector for
     * scratch space.
     */
    match.swap(roundMatch);
  }

  /* The final score is contained in the last slot of the round vector, which
   * corresponds to matching all characters of both strings.
   */
  return match.back();
}

#endif
