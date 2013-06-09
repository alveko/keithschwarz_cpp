/******************************************************************************
 * File: LightsOut.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A program that finds solutions to Lights Out puzzles.  The puzzle game
 * Lights Out, originally created in 1995, is a puzzle in which the player is
 * presented a rectangular grid of lights, some of which are lit.  The goal of
 * the game is to turn off all the lights.  To accomplish this, the player may
 * press the lights (which are also buttons) to toggle the state of that light
 * and each of the (up to four) surrounding lights.  For example, given the
 * grid
 *
 *                 . . . * .
 *                 . . . * *
 *                 . . . . .
 *
 * Pressing the fourth light on the second row would change the state of the
 * grid to be
 *
 *                 . . . . .
 *                 . . * . .
 *                 . . . * .
 *
 * This problem has been well-studied in the mathematical community because it
 * can be modeled and solved beautifully using a creative application of linear
 * algebra and finite fields.  The main observation one needs to have is that
 * the state of the board can be modeled as a matrix of 0s and 1s, where each
 * 0 means "light off" and each 1 means "light on."  For example, the above
 * board would be modeled as 
 *
 *                 0 0 0 0 0
 *                 0 0 1 0 0
 *                 0 0 0 1 0
 *
 * Given this, whenever the player pushes a button, we want to toggle some
 * set of lights, replacing 0s with 1s and vice-versa.  Since we want to map 0
 * to 1 and 1 to 0, we can think of toggling a light as equivalent to adding
 * 1 to that number, modulo 2.  For example, given the above grid, suppose that
 * we press the '1' in the center.  If we add one to the center light and its
 * surrounding lights, we get
 *
 *                 0 0 1 0 0
 *                 0 1 2 1 0
 *                 0 0 1 1 0
 *
 * which, modulo 2, looks like
 *
 *                 0 0 1 0 0
 *                 0 1 0 1 0
 *                 0 0 1 1 0
 *
 * which is the correct configuration of the lights at this point.
 *
 * The reason that this is interesting is that we can think of button toggles
 * in a slight different way.  In particular, since pushing each button is
 * equivalent to adding 1 (modulo 2) to the numbers around that button and
 * leaving the rest of the lights unchanged, we can think of each button as
 * being associated with a special matrix indicating what values to add to each
 * of the lights in the grid.  For example, the matrix for the center button is
 *
 *                0 0 1 0 0
 *                0 1 1 1 0
 *                0 0 1 0 0
 *
 * because if we press the center button, we toggle the five indicated lights
 * (by adding one modulo two) and leave the remaining lights unchanged (by
 * adding zero modulo two).  We can then simulating pressing a button by
 * simply adding the button's toggle matrix to the state of the world (modulo
 * two, of course).  Thus if we press the center button a second time in the
 * previous world configuration, we would get
 *
 *            0 0 1 0 0   0 0 1 0 0   0 0 2 0 0   0 0 0 0 0
 *            0 1 0 1 0 + 0 1 1 1 0 = 0 2 1 2 0 = 0 0 1 0 0
 *            0 0 1 1 0   0 0 1 0 0   0 0 2 1 0   0 0 0 1 0
 *
 * which is precisely what we want.
 *
 * The reason that this observation is interesting is that it gives us a very
 * clear, mathematical model for trying to solve the problem.  Suppose that our
 * game grid is an m x n matrix whose initial configuration is G.  Let the
 * toggle matrix for a button at (i, j) be T(i, j).  Given this, we want to
 * find a set of matrices such that
 *
 *     G + T(i1 j1) + T(i2, j2) + ... + T(ik, jk) = 0 (modulo 2)
 *
 * That is, we want a set of matrices that, summed together with G (modulo 2),
 * produce the zero matrix, in which all of the lights are turned off.
 *
 * A slightly different way of thinking about this is to assign to each toggle
 * matrix T(i, j) a coefficient a(i, j) that is either 0 or 1.  0 means that we
 * do not push the button, and 1 means that we do.  In that case, we are trying
 * to find a set of coefficients a(i, j) such that
 *
 *    G + sum a(i, j) T(i, j) = 0 (modulo 2)
 *        i,j
 *
 * Now, let's try to simplify things a bit.  Right now, it's a bit tricky to
 * do this math because everything in done modulo 2 and we have extra
 * constraints about the values of the coefficients a(i, j) (namely, that they
 * are either zero or one).  If you'll notice, all of the values that we're
 * working with are always zero or one, with addition and multiplication
 * defined modulo 2.  One way to think about this is to assume that we are not
 * working with integers, but rather elements of the field GF(2).  GF(2) (the
 * Galois field of order 2) is a mathematical structure called a field that
 * provides a formal definition of arithmetic modulo two.  In particular, it
 * defines arithmetic over zero and one by defining
 *
 *  - How to add numbers modulo two
 *  - How to multiply numbers modulo two
 *  - How to obtain the arithmetic inverse of a number modulo two
 *  - How to obtain the multiplicative inverse of a nonzero number modulo two
 *
 * These definitions are as follows:
 *
 *     0 + 0 = 0         0 * 0 = 0
 *     0 + 1 = 1         0 * 1 = 0
 *     1 + 0 = 1         1 * 0 = 0
 *     1 + 1 = 0         1 * 1 = 1
 *
 *      -0 = 0             1/1 = 1
 *      -1 = 1
 *
 * The rules for addition and multiplication of numbers modulo two probably
 * makes sense, but the rules for inverses is a bit tricky.  It should be
 * pretty clear why -0 = 0, but the rule -1 = 1 is not necessarily obvious. 
 * The reason that it is defined this way is that we want to have that
 *
 *     1 + (-1) = (-1) + 1 = 0
 *
 * Looking at the above table for arithmetic, we see that this is only possible
 * if -1 = 1.
 *
 * From the perspective of a computer scientist, it's worth noting that the
 * truth tables for addition and multiplication are, respectively, the same
 * truth tables for boolean XOR and AND.  This means that when we're working
 * with boolean values modulo 2, we can just use XOR and AND for arithmetic.
 * Also, since the arithmetic inverse of a number is always just that number
 * (as is the multiplicative inverse), we can invert numbers by just doing
 * nothing to them.
 *
 * Another important property of these definitions of arithmetic is that
 * multiplication distributes over addition, that is, a(b + c) = ab + ac.  The
 * reason that this is important is that it means that all of the usual
 * properties of arithmetic of real numbers apply to arithmetic modulo two.  In
 * particular, if we have a linear equation over those values, we can multiply
 * both sides of the equation by some value, add the same value to both sides,
 * etc.  Let's use this to simplify our original formulation of the solution to
 * the Lights Out game.  If you'll recall, we wanted to find a set of a(i, j)
 * drawn from 0 and 1 such that
 *
 *    G + sum a(i, j) T(i, j) = 0 (modulo 2)
 *        i,j
 *
 * If we assume that everything here is drawn from GF(2), then we can drop the
 * "modulo 2" to get
 *
 *    G + sum a(i, j) T(i, j) = 0
 *        i,j
 *
 * And we can now add -G to both sides of the equation to get
 *
 *    sum a(i, j) T(i, j) = -G
 *    i,j
 *
 * But remember that -x = x for all x in GF(2), so this is equivalent to
 *
 *    sum a(i, j) T(i, j) = G
 *    i,j
 *
 * Great!  All that's left to do now is to try to find the a(i, j) that we
 * need.  To do this, let's change our notation a bit.  Right now, G and the
 * T(i, j)'s are matrices.  We can easily conver these matrices into column
 * vectors by just looking at the values in row-major order.  For example, we
 * can rewrite T(1, 2) like this:
 *
 *    0 0 1 0 0
 *    0 1 1 1 0 -> (0 0 1 0 0 0 1 1 1 0 0 0 1 0 0)^T
 *    0 0 1 0 0
 *
 * If we do this, it becomes clearer that
 *
 *    sum a(i, j) T(i, j) = G
 *    i,j
 *
 * is just a linear system of equations over a set of vectors.  Let's define
 * the matrix V to be the matrix whose columns are the T(i, j) in some order
 * and the vector a to be the vector whose elements are the a(i, j) in that
 * same order.  If we do this, the above formula can be rewritten as
 *
 *    V a = G
 *
 * and from here our goal is clear - we just need to solve this system of
 * linear equations, each of whose values are drawn from GF(2) - and we have a
 * solution to the Lights Out puzzle!
 *
 * There is one important detail left now - how do we do this?  Fortunately,
 * because GF(2) is a field, we can just use standard Gaussian elimination to
 * solve for a value of a.  (If we knew that V was invertible, we would just
 * invert it, but we aren't always guaranteed that this is the case.)
 *
 * Now let's think about the runtime of the solution we've just come up with.
 * First, we need to compute the toggle matrices for the different buttons.
 * There are O(mn) of these, each of which takes time O(mn) to compute because
 * we have to fill in the vector with values.  This gives a runtime of
 * O(m^2 n^2) for this step.  Next, we have to perform the Gaussian elimination
 * to solve the linear system.  Let's see how long this will take.  At each
 * step, we need to scan the matrix for a column containing a nonzero value to
 * use as a pivot.  This takes O(mn) time per column and there are O(mn)
 * columns, giving us a time of O(m^2 n^2) just for this step.  When we find
 * the column to use, we then need to clear each other column.  This may
 * require looking at all O(m^2 n^2) elements of the matrix once for each of
 * the O(mn) columns, for a net runtime of O(m^3 n^3).  This dominates the
 * runtime, so the total complexity of the solver is O(m^3 n^3).  Since the
 * input has size O(mn), this runs in time proportional to the cube of the size
 * of the input.
 *
 * This code relies on the Matrix.hh header file also contained in the Archive
 * of Interesting Code.  You can find it at
 *
 *           http://www.keithschwarz.com/interesting/code/?dir=matrix
 */

#ifndef LightsOut_Included
#define LightsOut_Included

#include "Matrix.hh"
#include <vector>
#include <utility>    // For std::pair, std::make_pair
#include <algorithm>  // For std::fill, std::swap_ranges, std::swap,
                      //     std::transform
#include <stdexcept>  // For std::runtime_error
#include <functional> // For std::not_equal_to
#include <numeric>    // For std::accumulate

/**
 * Function: SolveLightsOut(const Matrix<M, N, bool>& puzzle);
 * ----------------------------------------------------------------------------
 * Given a game grid for Lights Out, returns a list of (row, col) pairs
 * indicating which buttons on the grid should be pressed in order to solve the
 * lights out puzzle.  The returned pairs are returned in no particular order.
 *
 * If the given puzzle cannot be solved, a std::runtime_error is thrown.
 *
 * Complexity: O(M^3 N^3)
 */
template <size_t M, size_t N>
std::vector< std::pair<size_t, size_t> >
SolveLightsOut(const Matrix<M, N, bool>& puzzle);

/* * * * * Implementation Below This Point * * * * */

namespace lightsout_detail {
  /**
   * Within this namespace, we always assume that we are dealing with row-major
   * order linearizations of matrices into vectors.
   */

  /**
   * Function: RowMajorIndex<N>(size_t i, size_t j)
   * --------------------------------------------------------------------------
   * Given an M x N matrix and a location (i, j) within that matrix, returns
   * the index at which that location may be found in a row-major ordering of
   * the elements of the array.  Since each step across a column just moves one
   * step further in the linearization and each step across a row moves past
   * the N elements of the row, this is given by i * N + j.
   */
  template <size_t N>
  size_t RowMajorIndex(size_t i, size_t j) {
    return i * N + j;
  }

  /**
   * Function: MakeToggleMatrix<M, N>();
   * --------------------------------------------------------------------------
   * Given the dimensions of a Lights Out puzzle, constructs the MN x MN matrix
   * whose columns are the toggle vectors for that puzzle.
   */
  template <size_t M, size_t N>
  Matrix<M * N, M * N, bool> MakeToggleMatrix() {
    /* The actual toggle matrix, which we initialize to hold 0 (false)
     * everywhere.
     */
    Matrix<M * N, M * N, bool> result;
    std::fill(result.begin(), result.end(), false);

    /* Now, for each of the locations in the matrix, see what happens when we
     * press that button.
     */
    for (size_t i = 0; i < M; ++i) {
      for (size_t j = 0; j < N; ++j) {
        /* Determine what column we're in.  This is found by seeing where in
         * the linearized array we should be.
         */
        size_t column = RowMajorIndex<N>(i, j);

        /* Set this position in the matrix to mark that pushing the button
         * toggles that light.
         */
        result[column][RowMajorIndex<N>(i, j)] = true;

        /* Iterate across the four neighbors and see whether they're in-bounds.
         * If so, mark that they're toggled by the button.
         */
        for (int di = -1; di <= +1; ++di) {
          for (int dj = -1; dj <= +1; ++dj) {
            /* We want exactly one of (di, dj) to be nonzero, and should skip
             * all other locations iterated over.
             */
            if ((di == 0) == (dj == 0)) continue;
            
            /* Although we're dealing with a mix of unsigned and signed values,
             * this math is acceptable because the result always ends up
             * unsigned.
             */
            if (i + di < M && j + dj < N)
              result[column][RowMajorIndex<N>(i + di, j + dj)] = true;
          }
        }
      }
    }
    return result;
  }

  /* Function: LinearizePuzzle(const Matrix<M, N, bool>& puzzle);
   * -------------------------------------------------------------------------
   * Linearizes a Lights Out puzzle into a vector in row-major order.
   */
  template <size_t M, size_t N>
  Vector<M * N, bool> LinearizePuzzle(const Matrix<M, N, bool>& puzzle) {
    /* The Vector constructor allows us to provide a set of iterators defining
     * the elements, and the Matrix type exports iterators to traverse the
     * elements in row-major order.  We just connect the two here.
     */
    return Vector<M * N, bool>(puzzle.begin(), puzzle.end());
  }
  
  /* Function: FindPivot(Matrix<M, N, bool>& matrix,
   *                     size_t startRow,
   *                     size_t pivotColumn);
   * -------------------------------------------------------------------------
   * Given a matrix and a starting row, looks for a column containing a pivot
   * (true value) in the indicated column.  If one is found, that row is
   * returned.  Otherwise, -1 is returned as a sentinel.
   */
  template <size_t M, size_t N>
  size_t FindPivot(Matrix<M, N, bool>& matrix,
                   size_t startRow,
                   size_t pivotColumn) {
    /* Scan down the rows of the matrix looking for the pivot. */
    for (size_t row = startRow; row < M; ++row)
      if (matrix[row][pivotColumn])
        return row;

    /* Didn't find anything. */
    return size_t(-1);
  }

  /* Function: PerformGaussianElimination(Matrix<MN, bool>& toggle,
   *                                      Vector<MN, bool>& puzzle);
   * -------------------------------------------------------------------------
   * Using Gaussian elimination, reduces the matrix 'toggle' to row echelon
   * form, performing corresponding operations on 'puzzle'.  All operations
   * are done in-place.
   */
  template <size_t MN>
  void PerformGaussianElimination(Matrix<MN, MN, bool>& toggle,
                                  Vector<MN, bool>& puzzle) {
    /* Keep track of the next free row at the top of the matrix where we can
     * swap a row containing a pivot.  As we find columns containing pivots,
     * we will pull the corresponding row up to the next free location.
     */
    size_t nextFreeRow = 0;

    /* Iterate across the columns of the matrix from left to right, looking
     * for a pivot element.
     */
    for (size_t col = 0; col < MN; ++col) {
      /* Find a row containing a pivot, then transform other rows if one is
       * found.
       */
      size_t pivotRow = FindPivot(toggle, nextFreeRow, col);

      /* If we couldn't find a pivot, go to the next column. */
      if (pivotRow == size_t(-1)) continue;

      /* Otherwise, bring this row up to the slot for the next free row. */
      std::swap_ranges(toggle.row_begin(pivotRow), toggle.row_end(pivotRow),
                       toggle.row_begin(nextFreeRow));

      /* Also, exchange the corresponding entries in the puzzle vector. */
      std::swap(puzzle[pivotRow], puzzle[nextFreeRow]);
      
      /* Now, for each row below this one that contains a 1 in the current
       * column, XOR this row with that row (since XOR is addition in GF(2))
       */
      for (size_t row = pivotRow + 1; row < MN; ++row) {
        /* If we need to clear this column, do so.  We accomplish this by
         * using transform to map XOR over this row and the row containing the
         * pivot.  For bools, XOR is equal to !=, so we map this operation
         * over both rows using transform and write the result back into the
         * row.
         */
        if (toggle[row][col]) {
          std::transform(
            /* Zip over the row containing the pivot... */   
            toggle.row_begin(nextFreeRow), toggle.row_end(nextFreeRow),

            /* ... and the row to clear... */
            toggle.row_begin(row),

            /* ... storing the result back into the row to clear... */
            toggle.row_begin(row),

            /* ... applying XOR. */
            std::not_equal_to<bool>()
          );

          /* Additionally, XOR the corresponding row of the puzzle vector. */
          puzzle[row] ^= puzzle[nextFreeRow];          
        }
      }
      
      /* Finally, update the position of the next free row. */
      ++nextFreeRow;
    }
  }

  /* Function: BackSubstitute(const Matrix<MN, MN, bool>& toggle,
   *                          const Vector<MN, bool>& puzzle);
   * -------------------------------------------------------------------------
   * Using back-substitution on the row-echelon matrix toggle augmented with
   * the column puzzle, returns a vector such that V a = G, where V is the
   * row-reduced toggle matrix and G is the puzzle vector.  If the system has
   * no solution, a std::runtime_error exception is raised.
   */
  template <size_t MN>
  Vector<MN, bool> BackSubstitute(const Matrix<MN, MN, bool>& toggle,
                                  const Vector<MN, bool>& puzzle) {
    /* Many of the puzzles we'll be solving have multiple solutions.  Because
     * of this, we need to pick some concrete one as our answer.  Since back-
     * substitution always assigns the last variables values before the first,
     * we will maintain our candidate solution vector initialized to all zero
     * values, then will update them accordingly.
     */
    Vector<MN, bool> result;
    std::fill(result.begin(), result.end(), false);

    /* Iterate from the bottom of the matrix to the top, updating our answer.
     * Because we want to iterate using unsigned values, we'll use a cute for
     * loop.  We will initialize the loop counter to MN, which is out of
     * bounds, and will then have our loop check include a test-and-decrement
     * to back it up one level.
     */
    for (size_t row = MN; row-- != 0; ) {
      /* Scan across the row to find the pivot, if one exists. */
      size_t pivot(-1);

      for (size_t col = 0; col < MN; ++col) {
        if (toggle[row][col]) {
          pivot = col;
          break;
        }
      }

      /* There are now two cases to check.  If this row is all zeros (i.e.
       * there's no pivot), we must check that the puzzle vector is false
       * here, since otherwise no solution exists.
       */
      if (pivot == size_t(-1)) {
        if (puzzle[row])
          throw std::runtime_error("Puzzle has no solution.");
      }
      else {
        /* Otherwise, update the value of this particular variable by using
         * the information in the rest of the row.  To do this, we know from
         * the positions of the remaining ones in the row and the value of the
         * puzzle vector that the value we should give to this variable is
         * one that satisfies
         *
         *     x_j + r[j+1] x_j+1 + ... + r[mn] x_mn = puzzleValue
         *
         * Subtracting those later values, we get
         *
         *     x_j = -r[j+1] x_j+1 + ... + -r[mn] x_mn + puzzleValue
         *
         * But because we're working in GF(2), -x == x for all x, so we get
         * that
         *     x_j = r[j+1] x_j+1 + ... + r[mn] x_mn + puzzleValue
         *
         * In other words, we just iterate across the rest of the row, XORing
         * the values together with the puzzle value and then store the
         * result.
         */
        result[row] = puzzle[row];
        for (size_t col = pivot + 1; col < MN; ++col)
          result[row] = (result[row] != (toggle[row][col] & result[col]));
      }
    }

    return result;
  }

  /* Function: SolvePuzzle(Matrix<MN, MN, bool> toggle,
   *                       Vector<MN, bool> puzzle);
   * -------------------------------------------------------------------------
   * Given a Lights Out puzzle and the toggle matrix, finds a solution to the
   * lights-out puzzle.
   *
   * We take the parameters by value because we will need to manipulate their
   * contents extensively within this function and don't want to destructively
   * modify the originals.
   */
  template <size_t MN>
  Vector<MN, bool> SolvePuzzle(Matrix<MN, MN, bool> toggle,
                               Vector<MN, bool> puzzle) {
    /* Use Gaussian elimination to reduce the toggle matrix and puzzle vector
     * to row echelon form.
     */
    PerformGaussianElimination(toggle, puzzle);

    /* Use back-substituion to construct a solution. */
    return BackSubstitute(toggle, puzzle);
  }

  /* Function: SolutionVectorToPairs<M, N>(const Vector<M*N, bool>& solution);
   * -------------------------------------------------------------------------
   * Given a vector containing the solution to a Lights Out puzzle encoded as
   * a column vector, expands the solution into a list of pairs.
   */
  template <size_t M, size_t N>
  std::vector< std::pair<size_t, size_t> >
  SolutionVectorToPairs(const Vector<M * N, bool>& solution) {
    std::vector< std::pair<size_t, size_t> > result;

    /* Iterate across the bits, rehydrating each. */
    for (size_t i = 0; i < M * N; ++i) {
      if (solution[i]) {
        /* Convert from an index back to a row/column pair.  To do this, we
         * use the quotient and the remainder of the raw index when divided by
         * the width of the matrix.
         */
        result.push_back(std::make_pair(i / N, i % N));
      }
    }
    
    return result;
  }
}

/* Actual implementation of the Lights Out solver. */
template <size_t M, size_t N>
std::vector< std::pair<size_t, size_t> >
SolveLightsOut(const Matrix<M, N, bool>& puzzle) {
  /* Begin by constructing the matrix V consisting of all the toggle vectors
   * for the puzzle.
   */
  Matrix<M * N, M * N, bool> toggle = 
    lightsout_detail::MakeToggleMatrix<M, N>();

  /* Linearize the puzzle into a vector so that we can swap its rows in the
   * same way that we would sway the rows of the matrix.
   */
  Vector<M * N, bool> puzzleVector = lightsout_detail::LinearizePuzzle(puzzle);

  /* Using Gaussian elimination, obtain a solution to the original problem. */
  Vector<M * N, bool> solution =
    lightsout_detail::SolvePuzzle(toggle, puzzleVector);

  /* Convert the solution vector into a list of the buttons to toggle. */
  return lightsout_detail::SolutionVectorToPairs<M, N>(solution);
}

#endif
