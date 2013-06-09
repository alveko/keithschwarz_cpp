/******************************************************************************
 * File: Strassen.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of Strassen's algorithm for fast matrix multiplication.
 * While a naive matrix multiply takes O(N^3) to multiply square matrices of
 * size N x N, Strassen's algorithm runs in time O(N^(lg 7)) ~= O(N^2.8).  It
 * manages to do so by cleverly computing a family of smaller matrix products
 * that can be used to recover all the multiplications necessary to compute the
 * matrix product.
 *
 * To see how the algorithm works, let's begin by assuming that we're dealing
 * with matrices that are of size 2^k x 2^k for some k >= 1.  Then if we have
 * the product C = AB, we can rewrite this as
 *
 *              | C_00  C_01 |   | A_00  A_01 | | B_00  B_01 |
 *              | C_10  C_11 | = | A_10  A_11 | | B_10  B_11 |
 *
 * And therefore
 *
 *                        C_00 = A_00 B_00 + A_01 B_10
 *                        C_01 = A_00 B_01 + A_01 B_11
 *                        C_10 = A_10 B_00 + A_11 B_10
 *                        C_11 = A_10 B_01 + A_11 B_11
 *
 * This involves making eight multiplications of matrices of size 2^(k-1) and
 * doing a total of four additions.  We can then get a runtime of
 *
 *                          T(n) <= 8T(n / 2) + O(n^2)
 *
 * Using the Master Theorem, this recurrence expands out to O(n^(lg 8)) = 
 * O(n^3).
 *
 * Strassen's key insight was twofold.  First, if we do use a divide-and-
 * conquer algorithm to compute matrix products, we can perform a constant
 * number of matrix additions at each step without affecting the asymptotic
 * runtime of the algorithm.  If you'll notice, in the above recurrence for
 * T(N), as long as the amount of time spent doing additions is O(N^2), we'd
 * arrive at the same recurrence.  The second observation is that the eight
 * matrix products we need to compute to find the C_ij's can be computed
 * indirectly by instead computing the products of seven other matrices that
 * individually do not give the answer, but whose sums or differences do.  In
 * this way, it's possible to eliminate an unnecessary multiplication by
 * instead doing many more matrix additions and subtractions.
 *
 * More formally, Strassen's algorithm works by computing the following seven
 * matrix products:
 *
 *                      M_0 = (A_00 + A_01) B_11
 *                      M_1 = A_00 (B_01 - B_11)
 *                      M_2 = (A_10 + A_11) B_00
 *                      M_3 = A_11 (B_10 - B_00)
 *                      M_4 = (A_10 - A_00) (B_00 + B_01)
 *                      M_5 = (A_00 + A_11) (B_00 + B_11)
 *                      M_6 = (A_01 - A_11) (B_10 + B_11)
 *
 * Notice that
 *
 * M_5 + M_6 - M_0 + M_3 = +1 (A_00 + A_11) (B_00 + B_11)
 *                         +1 (A_01 - A_11) (B_10 + B_11)
 *                         -1 (A_00 + A_01) B_11
 *                         +1 (B_10 - B_00) A_11
 *                       = +1 (A_00 B_00 + A_00 B_11 + A_11 B_00 + A_11 B_11)
 *                         +1 (A_01 B_10 + A_01 B_11 - A_11 B_10 - A_11 B_11)
 *                         -1 (A_00 B_11 + A_01 B_11)
 *                         +1 (A_11 B_10 - A_11 B_00)
 *                       = (A_00 B_00 + A_00 B_11 + A_11 B_00 + A_11 B_11)
 *                         (A_01 B_10 + A_01 B_11 - A_11 B_10 - A_11 B_11)
 *                         (-A_00 B_11 + -A_01 B_11)
 *                         (A_11 B_10 - A_11 B_00)
 *                       = A_00 B_00 + A_01 B_10
 *                       = C_00
 *
 * M_0 + M_1 = A_00 B_11 + A_01 B_11 + A_00 B_01 - A_00 B_11
 *           = A_00 B_01 + A_01 B_11
 *           = C_01
 *
 * M_2 + M_3 = A_10 B_00 + A_11 B_00 + A_11 B_10 - A_11 B_00
 *           = A_10 B_00 + A_11 B_10
 *           = C_10
 *
 * M_4 + M_5 + M_1 - M_2 = +1 (A_10 - A_00) (B_00 + B_01)
 *                         +1 (A_00 + A_11) (B_00 + B_11)
 *                         +1 (B_01 - B_11) A_00
 *                         -1 (A_10 + A_11) B_00
 *                       = +1 (A_10 B_00 + A_10 B_01 - A_00 B_00 - A_00 B_01)
 *                         +1 (A_00 B_00 + A_00 B_11 + A_11 B_00 + A_11 B_11)
 *                         +1 (A_00 B_01 - A_00 B_11)
 *                         -1 (A_10 B_00 + A_11 B_00)
 *                       = (A_10 B_00 + A_10 B_01 - A_00 B_00 - A_00 B_01)
 *                         (A_00 B_00 + A_00 B_11 + A_11 B_00 + A_11 B_11)
 *                         (A_00 B_01 - A_00 B_11)
 *                         (-A_10 B_00 - A_11 B_00)
 *                       = A_10 B_01 + A_11 B_11
 *                       = C_11
 *
 * In other words, these seven matrices are sufficient to recover all the C_ij
 * and thus the entire matrix product.
 *
 * There are two cases we still need to address.  First, this recursion needs
 * a base case, since otherwise we'll just keep splitting indefinitely.  This
 * part is easy.  One option would be to stop as soon as the submatrices hit
 * size 1x1, in which case the multiplications are just straight scalar
 * multiplications.  A more efficient solution, and the one adopted in this
 * implementation, is to pick some cutoff size (say, n = 4) and then to just
 * do a straight matrix multiply whenever the matrices become this size or
 * smaller.
 *
 * The other issue to consider is how to handle the case where the matrices
 * aren't perfectly square matrices of size 2^k for some k.  We can handle
 * this case by padding each matrix until it does meet this requirement, then
 * doing the multiplication, and finally extracting the resulting matrix from
 * the padded product.  If the input matrices are of size M x P and P x N,
 * this approach takes time O(max{M, N, P}^{lg 7}).
 *
 * This code relies on the Matrix class provided by the Archive of Interesting
 * Code.  You can find it at
 *
 *           http://www.keithschwarz.com/interesting/code/?dir=matrix
 */

#ifndef Strassen_Included
#define Strassen_Included

#include "Matrix.hh"
#include <algorithm> // For std::fill

/**
 * Matrix<M, N> StrassenProduct(const Matrix<M, P>& lhs, const Matrix<P, N>& rhs);
 * Usage: AB = StrassenProduct(A, B);
 * ---------------------------------------------------------------------------
 * Computes the product of two matrices uses Strassen's algorithm.
 */
template <size_t M, size_t N, size_t P, typename T>
const Matrix<M, N, T> StrassenProduct(const Matrix<M, P, T>& lhs,
                                      const Matrix<P, N, T>& rhs);

/* * * * * Implementation Below This Point * * * * */
namespace strassen_detail {
  /* Function: SquareStrassenProduct(const Matrix<N, N, T>& lhs,
   *                                 const Matrix<N, N, T>& rhs);
   * Usage: AB = SquareStrassenProduct(A, B);
   * -------------------------------------------------------------------------
   * Computes the product of two square matrices (whose sizes are assumed to
   * be perfect powers of two) using Strassen's algorithm.
   */
  template <size_t N, typename T>
  const Matrix<N, N, T> SquareStrassenProduct(const Matrix<N, N, T>& lhs,
                                              const Matrix<N, N, T>& rhs) {
    /* Base case: If the matrices are sufficiently small, just return their
     * product using the naive algorithm.
     */
    if (N <= 1)
      return lhs * rhs;

    /* Otherwise, extract four square submatrices from each of the input
     * matrices.  These are matrices of dimension N/2 by N/2.
     */
    Matrix<N/2, N/2, T> A[2][2], B[2][2];

    /* Fill in these matrices with the proper values.  These outer loops count
     * across the quadrants to scan, while the inner loops scan across the
     * contents of those quadrants.
     */
    for (size_t i = 0; i < 2; ++i) {
      for (size_t j = 0; j < 2; ++j) {
        for (size_t x = 0; x < N/2; ++x) {
          for (size_t y = 0; y < N/2; ++y) {
            /* Copy the contents of lhs into the corresponding A block.  I've
             * used the .at() function to fill in the matrix instead of using
             * the square brackets to make it clearer that A[i][j] is the
             * submatrix in question and (x, y) is the index.
             */
            A[i][j].at(x, y) = lhs[i * N/2 + x][j * N/2 + y];

            /* Similar logic for the rhs matrix. */
            B[i][j].at(x, y) = rhs[i * N/2 + x][j * N/2 + y];
          }
        }
      }
    }

    /* Compute the seven products necessary for the result. */
    const Matrix<N/2, N/2, T> 
      M0 = SquareStrassenProduct(A[0][0] + A[0][1], B[1][1]),
      M1 = SquareStrassenProduct(A[0][0], B[0][1] - B[1][1]),
      M2 = SquareStrassenProduct(A[1][0] + A[1][1], B[0][0]),
      M3 = SquareStrassenProduct(A[1][1], B[1][0] - B[0][0]),
      M4 = SquareStrassenProduct(A[1][0] - A[0][0], B[0][0] + B[0][1]),
      M5 = SquareStrassenProduct(A[0][0] + A[1][1], B[0][0] + B[1][1]),
      M6 = SquareStrassenProduct(A[0][1] - A[1][1], B[1][0] + B[1][1]);
    
    /* From this, compute the C_ij matrices corresponding to the components of
     * the result.
     */
    const Matrix<N/2, N/2, T> C[2][2] = { { M5 + M6 - M0 + M3, M0 + M1},
                                          { M2 + M3, M4 + M5 + M1 - M2} };
        
    /* Finally, compose this back up into one large result matrix. */
    Matrix<N, N, T> result;

    /* This for loop is essentially the inverse of the loop for splitting the
     * matrices into components.
     */
    for (size_t i = 0; i < 2; ++i)
      for (size_t j = 0; j < 2; ++j)
        for (size_t x = 0; x < N/2; ++x)
          for (size_t y = 0; y < N/2; ++y)
            result[i * N/2 + x][j * N/2 + y] = C[i][j].at(x, y);

    return result;
  }

  /* Metafunction: Log2<N>
   * Usage: Log2<N>::value
   * -------------------------------------------------------------------------
   * A metafunction that computes floor(lg N).
   */
  template <size_t N> struct Log2; 

  /* floor(lg 1) == 0 */
  template <> struct Log2<1> {
    static const size_t value = 0;
  };

  /* floor(lg N) = 1 + floor(lg(N / 2)) */
  template <size_t N> struct Log2 {
    static const size_t value = 1 + Log2<N/2>::value;
  };

  /* Metafunction: MatrixSize<N>
   * Usage: MatrixSize<N>::value
   * -------------------------------------------------------------------------
   * A metafunction which, given as input N, the length of a side of a matrix,
   * returns the side length of a matrix that should be used when the matrix
   * is input into Strassen's algorithm.  For powers of two, this is the size
   * of the input, and for other values is the smallest power of two greater
   * than the input size.
   */
  template <size_t N> struct MatrixSize;

  /* If the input matrix has size 0, the input matrix should have size
   * zero.
   */
  template <> struct MatrixSize<0> {
    static const size_t value = 0;
  };
  /* Otherwise, compute 2^floor(lg N).  If this equals the input size, then
   * just hand that back.  Otherwise, return twice that value.
   */
  template <size_t N> struct MatrixSize {
    static const size_t value = 
      (1 << Log2<N>::value == N)? N : (1 << (Log2<N>::value + 1));
  };

  /* Metafunction: Max<M, N>
   * Usage: Max<137, 42>::value
   * -------------------------------------------------------------------------
   * Returns the larger of the two inputs.
   */
  template <size_t M, size_t N> struct Max {
    static const size_t value = M >= N? M : N;
  };
}

/* Actual implementation of the Strassen product. */
template <size_t M, size_t N, size_t P, typename T>
const Matrix<M, N, T> StrassenProduct(const Matrix<M, P, T>& lhs,
                                      const Matrix<P, N, T>& rhs) {
  /* Give access to all of the utility functions and templates from above. */
  using namespace strassen_detail;

  /* Begin by finding the smallest square matrix that can hold both matrices
   * while having power-of-two side lengths.
   */
  static const size_t MaxDim = Max<Max<M, N>::value, P>::value;
  static const size_t Size = MatrixSize<MaxDim>::value;
  typedef Matrix<Size, Size, T> InputMatrix;

  /* Create two matrices of this size, filling them with zeros. */
  InputMatrix newLhs, newRhs;
  std::fill(newLhs.begin(), newLhs.end(), T(0));
  std::fill(newRhs.begin(), newRhs.end(), T(0));

  /* Copy over the matrix contents. */
  for (size_t i = 0; i < M; ++i)
    for (size_t j = 0; j < P; ++j)
      newLhs[i][j] = lhs[i][j];
  for (size_t i = 0; i < P; ++i)
    for (size_t j = 0; j < N; ++j)
      newRhs[i][j] = rhs[i][j];

  /* Fire off a call to Strassen's algorithm in the square case. */
  const InputMatrix product = SquareStrassenProduct(newLhs, newRhs);

  /* Recover the resulting matrix as a submatrix. */
  Matrix<M, N, T> result;
  for (size_t i = 0; i < M; ++i)
    for (size_t j = 0; j < N; ++j)
      result[i][j] = product[i][j];

  /* Hand back this submatrix. */
  return result;
}

#endif
