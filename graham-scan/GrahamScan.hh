/*****************************************************************************
 * File: GrahamScan.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An algorithm for finding the convex hull of a set of points in the 2D plane
 * using the Graham Scan.  This algorithm has very good practical runtime
 * (O(n lg n)) and is fairly simple to understand.  The intuition behind the
 * algorithm is to find some point that is known to be on the convex hull of
 * the list of points, then to compute the angles between that point and every
 * other point in the set.  From there, we can sort the points in O(n lg n)
 * time.  The algorithm concludes by marching around these points in sorted
 * order, checking whether each is on the convex hull and adding it if it is.
 *
 * More specifically, the algorithm begins by picking the point with the
 * smallest Y coordinate, which must be on the convex hull.  It then computes
 * the angle made by each other point in the set and the X axis, then sorts
 * these points in ascending order.  Next, we maintain a stack of our current
 * guess of what the convex hull is, which initially is the point with the
 * lowest Y value and the point with the lowest (signed) angle with the x
 * axis.  From there, we iterate across the points in the convex hull
 * expanding our guess.  In particular, let v0 and v1 be the last two points
 * in our convex hull estimate, and let v2 be the next test point.  We then
 * compute the angle between the vector v1 - v0 and the vector v2 - v1.  If
 * this angle is in [pi, 2pi), then we are in a situation like this:
 *
 *                                             /
 *                                v1          /
 *                                *----------*
 *                               /          v0
 *                              /
 *                             /
 *                            *
 *                           v2
 *
 * This means that the point v1 is not actually on the convex hull; it's
 * contained in the hull between v0 and v2.  Of course, depending on what the
 * point in our convex hull estimate is that comes before v0, it's possible
 * that v0 itself isn't on the convex hull either.  In this case, we continue
 * removing nodes from our convex hull estimate until we find that the angle
 * between the last two nodes in the estimate and the new node is in the
 * range [0, pi).  We then update the convex hull by appending this new
 * point.
 *
 * We can now argue the correctness of the algorithm, along with the O(n lg n)
 * runtime.  We can easily see that the algorithm produces a convex polygon,
 * since if we follow the edges from the starting vertex v0 around, each edge
 * turns inward toward the center of the polygon. (A more rigorous proof of
 * this fact exists, but I think this previous one is more intuitive).  To see
 * that the algorithm produces a convex polygon containing all the points in
 * the initial input set, suppose for the sake of contradiction that this
 * isn't true; that there is some node v that isn't in the hull.  It can't be
 * lower than v0, since v0 has the lowest y coordinate, and so it must have
 * been considered by the algorithm at some point.  This means that it was
 * added to the convex hull candidate, and because it wasn't returned it must
 * have been removed at some point.  This means that there was some new point
 * in which the angle between the edge ending at v and the edge containing the
 * new point was in [0, pi).  But by removing this node from consideration,
 * the new edge added between the predecessor of v and the new point has point
 * v in its negative half-space, and so v is in the hull, a contradiction.
 *
 * To argue that the runtime is O(n lg n), we note that we can find the point
 * with the smallest y coordinate in O(n) time, compute the angle each point
 * makes with the x axis in constant time per node (taking O(n) net time), and
 * can then sort them in O(n lg n).  If we can show that the step of growing
 * the hull takes O(n lg n) time, then the overall runtime will be O(n lg n).
 * Initially, it might not seem that this step of the algorithm runs in this
 * time.  Each time we add a node we may have to backtrack all the way through
 * the potentially O(n) nodes on the convex hull, and since we're considering
 * O(n) nodes, this takes O(n^2) time.  However, this analysis is not tight.
 * Notice that once we remove a node from the stack, no future backtracking
 * can ever visit this node again.  This means that in all of the backtracking
 * operations, we can remove at most O(n) nodes from the stack.  Since each
 * backtracking operation takes time linear in the number of nodes removed,
 * this means that the total runtime for all of the backtracking operation is
 * O(n), giving us an overall runtime of O(n lg n).
 *
 * This code relies on the Matrix.hh header file also contained in the Archive
 * of Interesting Code.  You can find it at
 *
 *           http://www.keithschwarz.com/interesting/code/?dir=matrix
 */

#ifndef GrahamScan_Included
#define GrahamScan_Included

#include "Matrix.hh" // For Vector, DotProduct, Norm, NormSquared, CrossProduct
#include <iterator>  // For distance
#include <algorithm> // For copy, min_element, sort
#include <vector>

/**
 * Function: GrahamScan(ForwardIterator begin, ForwardIterator end,
 *                      OutputIterator out);
 * Usage: GrahamScan(pts.begin(), pts.end(), back_inserter(convexHull);
 * ---------------------------------------------------------------------------
 * Given a range of iterators [begin, end) spanning a range of Vector<2>s that
 * encode points in space, produces the convex hull of those points using the
 * Graham Scan algorithm.  The points are stored in counter-clockwise order
 * around the convex hull, so the resulting hull is the intersections of the
 * positive half-spaces of all the edges.  The result is stored in the 
 * sequence beginning with out, which is assumed to have enough space to hold
 * the resulting sequence.  The return value is an iterator one past the 
 * location of the last value written.
 */
template <typename ForwardIterator, typename OutputIterator>
OutputIterator GrahamScan(ForwardIterator begin, ForwardIterator end,
                          OutputIterator out);

/* * * * * Implementation Below This Point * * * * */
namespace grahamscan_detail {
  /**
   * Function: CompareYCoordinates(const Vector<2>& lhs, const Vector<2>& rhs)
   * Usage: if (CompareYCoordinates(pt1, pt2)) { ... }
   * -------------------------------------------------------------------------
   * Compares two vectors by their y coordinates, returning whether the lhs
   * has a y coordinate strictly less than the rhs's y coordinate.  In the
   * event of a tie, they are then compared by their x coordinate.
   */
  bool CompareYCoordinates(const Vector<2>& lhs, const Vector<2>& rhs);

  /**
   * Functor: CompareByAngle(const Vector<2>& lhs, const Vector<2>& rhs);
   * Usage: if (CompareByAngle(lhs, rhs)) { ... }
   * -------------------------------------------------------------------------
   * A functor class that sorts points according to the angle that they make
   * with the X axis.  All comparisons are made assuming that the points are
   * in a coordinate system that is a translated to some new origin.
   */
  class CompareByAngle {
  public:
    /**
     * Constructor: CompareByAngle(const Vector<2>& origin)
     * Usage: std::sort(begin, end, CompareByAngle(origin))
     * -----------------------------------------------------------------------
     * Constructs a new comparator with the indicated point as the origin.
     */
    explicit CompareByAngle(const Vector<2>& origin);

    /**
     * bool operator() (const Vector<2>& lhs, const Vector<2>& rhs) const
     * Usage: myComp(pt1, pt2);
     * -----------------------------------------------------------------------
     * Compares lhs and rhs, returning whether lhs makes a smaller angle with
     * the origin than rhs.  If lhs and rhs make the same angle, the distance
     * from the origin to these points is used as a tiebreaker, with the
     * closer point winning the tiebreaker.
     */
    bool operator() (const Vector<2>& lhs, const Vector<2>& rhs) const;

  private:
    const Vector<2> origin;
  };
}

/* Actual implementation of the Graham scan. */
template <typename ForwardIterator, typename OutputIterator>
OutputIterator GrahamScan(ForwardIterator begin, ForwardIterator end,
                          OutputIterator out) {
  /* Grant access to all of the utility functions and classes from above. */
  using namespace grahamscan_detail;

  /* Edge cases - if the range has fewer than three elements, the convex hull
   * is just those points.
   */
  if (size_t(std::distance(begin, end)) < 3)
    return std::copy(begin, end, out);

  /* Locate the element with the smallest y value, breaking ties by choosing
   * coordinates as far to the right (-x) as possible.
   */
  ForwardIterator minY = std::min_element(begin, end, CompareYCoordinates);

  /* Get an iterator one step past minY; it's the start of the sequence of
   * values that come after it in the input.
   */
  ForwardIterator next = minY; ++next;

  /* We now need to sort the points by their angle with the X axis.  Because
   * we aren't allowed to rearrange the input sequence, we'll make a local
   * copy of the sequence, then will sort that.  We'll leave the lowest point
   * out of the copy so that we don't end up including it in the result.
   */
  std::vector< Vector<2> > points;
  points.insert(points.end(), begin, minY); // First portion of the points.
  points.insert(points.end(), next, end);   // Remainder of the points.

  /* Sort by angle with the X axis.  To avoid issues where two adjacent points
   * in the sequence have an 180 degree angle between them, break ties by
   * choosing the point closest to the bottommost point.
   */
  std::sort(points.begin(), points.end(), CompareByAngle(*minY));

  /* For simplicity, add the minimum point onto the end of the ordering.  This
   * allows us to confirm that the last point we add in the sweep is correct
   * without having to special-case it.
   */
  points.push_back(*minY);

  /* Now, start building up the list of the points in the convex hull.
   * Initially this is the lowest point and the point with the lowest angle,
   * which happens to be the first element of the sorted sequence.
   */
  std::vector< Vector<2> > result;
  result.push_back(*minY);
  result.push_back(points[0]);

  /* Now, continuously refine the convex hull until we end up coming back
   * around to the beginning.
   */
  for (size_t i = 1; i < points.size(); ++i) {
    /* Expand the convex hull by factoring in this next point.  This may
     * entail removing some of our previous points, but it always ends by
     * adding this new point.
     */
    while (true) {
      /* Compute two vectors - one consisting of the last two points of the
       * candidate hull, and one consisting of of the last point and the next
       * point in the list.
       */
      const Vector<2> last = result[result.size() - 1] - result[result.size() - 2];
      const Vector<2> curr = points[i] - result[result.size() - 1];

      /* Check whether the angle between these vectors is in the range [0, pi)
       * or [pi, 2*pi).  If it's in the first group, we can add it.  Otherwise
       * we need to remove the last point from the hull and try again.
       *
       * Rather than directly computing the angle between the two vectors, we
       * can instead compute the sine of the angle.  If it's between [0, pi)
       * this will be nonnegative, and if it's between [pi, 2*pi) this would
       * be negative.
       *
       * We can compute the sine of the angle between the vectors by using the
       * 2D cross-product:
       *
       *             |   1   1   1 |
       *   |A x B| = | A.x A.y   0 | = A.x B.y - A.y B.x = |A| |B| sin(theta)
       *             | B.x B.y   0 |
       *
       * Since |A| |B| >= 0, this quantity is positive iff sin(theta) is
       * positive.
       */
      if (last[0] * curr[1] - last[1] * curr[0] >= 0) break;

      /* If we're here, it means that this angle was negative and so our last
       * point isn't going to work.  Undo it.
       */
      result.pop_back();
    }

    /* Finally, add the point. */
    result.push_back(points[i]);
  }

  /* At the very end, we now have our convex hull, with the lowest point added
   * twice.  We'll get rid of this point, then return the hull we found.
   */
  result.pop_back();

  /* Move the hull into the output range, then return the endpoint. */
  return std::copy(result.begin(), result.end(), out);
}

#endif
