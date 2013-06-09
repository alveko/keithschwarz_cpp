/******************************************************************************
 * File: PowerIteration.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the power iteration algorithm for finding the dominant
 * eigenvalue (and an associated eigenvector) of a matrix.  Power iteration is
 * one of the simplest algorithms for finding eigenvalues, and accordingly it
 * does not have the best convergence guarantees.  However, it is clean,
 * simple, and in many cases quite elegant.
 *
 * The idea behind power iteration is to start off with a guess of a unit
 * eigenvector for the matrix, which we'll call x0.  We then compute the series
 *
 *                                    Ax_{n}
 *                       x_{n+1} =  ----------
 *                                  ||Ax_{n}||
 *
 * That is, we apply the matrix A to the vector, then normalize it, and repeat.
 * Over time, if the dominant eigenvector of A is sufficiently larger than the
 * other eigenvalues of the matrix, the estimates will get closer and closer to
 * the target.
 *
 * The precise mathematical proof of correctness of this algorithm is fairly
 * complex in the general case, but in the specific case where the matrix is
 * diagonalizable it's actually quite elegant.  Given a matrix A, suppose that
 * we can diagonalize the matrix as A = ULU*.  Then we have that A^n = UL^nU*,
 * and so A^n x = UL^nU*x.  Consider the magnitude of this right-hand side,
 * ||UL^nU*x||.  Since U and U* are unitary, this is ||L^n x||.  Letting 
 * x = (x0, x1, ..., xk) and letting the diagonal of L be L0, L1, ..., Lk, this
 * expression is
 *
 *                   || (L0^n x0, L1^n x1, ..., Lk^n xk) ||
 *
 * Assuming that we order the diagonal of L such that L0 >= L1 >= ... >= Lk,
 * the magnitude of this first term grows much faster than the rest of the
 * terms of the vector.  Since A^n x = UL^nU*x, this means that the result of
 * A^n x is equivalent to rewriting x in the eigenbasis, dramatically scaling
 * its first component (much more than the other components), and then
 * converting it back.  Over many iterations, this ultimately causes the result
 * of this process to converge to an eigenvector associated with L0.
 *
 * This code relies on the Matrix.hh header file also contained in the Archive of
 * Interesting Code.  You can find it at
 *
 *           http://www.keithschwarz.com/interesting/code/?dir=matrix
 */

#ifndef PowerIteration_Included
#define PowerIteration_Included

#include "Matrix.hh"
#include <utility>   // For pair

/**
 * std::pair< T, Vector<N, T> >  PowerIteration(const Matrix<N, N, T>& A,
 *                                              const Vector<N, T>& initialGuess,
 *                                              size_t numIterations);
 * Usage: std::pair< double, Vector<3> > eigen = PowerIteration(A, guess, 100);
 * ----------------------------------------------------------------------------
 * Given a matrix A, an initial guess of an eigenvector, and a number of
 * iterations, runs the power iteration algorithm on that matrix to approximate
 * an eigenvalue/eigenvector pair.  If at some point during the algorithm's
 * execution the current estimate becomes the zero vector, the returned pair
 * will have an appropriate value flagged in the return value.
 */
template <size_t N, typename T>
std::pair< T, Vector<N, T> > PowerIteration(const Matrix<N, N, T>& A,
                                            const Vector<N, T>& initialGuess,
                                            size_t numIterations) {
  /* Set our initial guess to be the unit vector pointing in the specified
   * direction.
   */
  Vector<N, T> currGuess = initialGuess / Norm(initialGuess);

  /* Run the power iteration the indicated number of times. */
  for (size_t i = 0; i < numIterations; ++i) {
    /* Apply A to the vector, then normalize it. */
    currGuess = A * currGuess;
    currGuess /= Norm(currGuess);
  }

  /* We now have a candidate eigenvector.  To get its corresponding eigenvalue,
   * we can compute ||Ax|| / ||x||.  However, since we already know that x is
   * a unit vector, this is equivalent to computing ||Ax||.
   */
  return std::make_pair(Norm(A * currGuess), currGuess);
}

#endif
