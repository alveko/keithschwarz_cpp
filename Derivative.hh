/*****************************************************************************
 * File: Derivative.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A function that produces a function object containing an approximation of
 * the derivative of a real-valued function.  Internally, the function works
 * by handing back a function object that uses a secant-line approximation of
 * the derivative, as shown here:
 *
 *            f(x + e) - f(x - e)
 * f'(x) ~=   -------------------
 *                     2e
 *
 * This can be shown to produce the derivative with a very good approximation
 * for small e.
 */
#ifndef Derivative_Included
#define Derivative_Included

/* Forward declarations to make TakeDerivative make sense. */
namespace derivative_detail {
  template <typename UnaryFunction> class Derivative;
}

/**
 * Function: TakeDerivative(UnaryFunction function, double epsilon = 0.000001);
 * Usage: transform(v.begin(), v.end(), v.begin(), TakeDerivative(sinc));
 * --------------------------------------------------------------------------
 * Given a unary function (and optionally an epsilon), returns a function object
 * containing an approximation of the first derivative of that function.
 */
template <typename UnaryFunction>
derivative_detail::Derivative<UnaryFunction>
TakeDerivative(UnaryFunction function, double epsilon = 0.000001);

/* * * * * Implementation Below This Point * * * * */
namespace derivative_detail {
  /* Functor class responsible for approximating a derivative. */
  template <typename UnaryFunction> class Derivative {
  public:
    /* Constructor stores the function and epsilon to use. */
    Derivative(UnaryFunction function, double epsilon) 
      : mFunction(function), mEpsilon(epsilon) {
      // Handled in initializer list.
    }

    /* Functor operator produces a derivative approximation. */
    double operator() (double x) const {
      return (mFunction(x + mEpsilon) - mFunction(x - mEpsilon)) / (2.0 * mEpsilon);
    }

  private:
    /* The function to call and the obligatory epsilon. */
    UnaryFunction mFunction;
    double mEpsilon;
  };
}

/* Actual implementation of TakeDerivative. */
template <typename UnaryFunction>
derivative_detail::Derivative<UnaryFunction>
TakeDerivative(UnaryFunction function, double epsilon) {
  /* Construct a Derivative of the correct type, then hand it back. */
  return derivative_detail::Derivative<UnaryFunction>(function, epsilon);
}

#endif
