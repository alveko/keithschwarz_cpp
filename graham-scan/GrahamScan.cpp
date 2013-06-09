/*****************************************************************************
 * File: GrahamScan.cpp
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of the template-independent helper functions necessary for
 * the Graham scan to function properly.
 */
#include "GrahamScan.hh"

namespace grahamscan_detail {
  /* Comparing by Y values first looks at Y, then at X. */
  bool CompareYCoordinates(const Vector<2>& lhs, const Vector<2>& rhs) {
    /* If the Y coordinates are unequal, return whether the lhs has a smaller
     * value than the rhs.
     */
    if (lhs[1] != rhs[1])
      return lhs[1] < rhs[1];

    /* Otherwise, fall back to comparing by x coordinates. */
    return lhs[0] < rhs[0];
  }

  /**** CompareByAngle Implementation ****/
  
  /* Constructor stores the specified origin. */
  CompareByAngle::CompareByAngle(const Vector<2>& origin) : origin(origin) {
    // Handled by initializer list
  }

  /* Comparison function compares first by angle, then by distance. */
  bool CompareByAngle::operator() (const Vector<2>& lhs, 
                                   const Vector<2>& rhs) const {
    /* Begin by translating the two vectors to the local coordinate system. */
    const Vector<2> one = lhs - origin;
    const Vector<2> two = rhs - origin;

    /* Now, for a cool optimization.  We're curious about whether the first
     * vector makes a lower angle with the x axis than the second.  However,
     * don't really care about the angle; we can apply any monotonically
     * increasing function we want to the angle and then compare that.  In
     * particular, one function we could look at is -cos(theta).  Because
     * the origin we're using is always the point with the lowest y
     * coordinate, we know that the cosine of the angle is always between
     * [0, pi), and so -cos is a monotonically increasing function over this
     * range.
     *
     * The reason for doing this is that we can compute -cos(theta) as
     *
     *                  -cos(theta) = -(A . B) / (|A||B|)
     *
     * To get cos(theta) between the vectors A and B.  In our case, we'll pick
     * our vector A to be unit vector along the x axis (that is, (1, 0)), and
     * letting B be the vector in consideration, this simplifies down to
     *
     *          -cos(theta) = -(1 * B.x + 0 * B.y) / |B| = -B.x / |B|
     *
     * Which can be computed extremely efficiently.
     */
    const double normOne = Norm(one);
    const double normTwo = Norm(two);
    const double negCosOne = -one[0] / normOne;
    const double negCosTwo = -two[0] / normTwo;

    if (negCosOne != negCosTwo)
      return negCosOne < negCosTwo;

    /* If the two angles are the same, return whichever is closer. */
    return normOne < normTwo;
  }
}
