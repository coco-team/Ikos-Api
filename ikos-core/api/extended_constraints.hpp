/********************************************************************************
 *
 * Data structures for the symbolic manipulation of extended linear constraints.
 *
 * Author: Maxime Arthaud (maxime@arthaud.me)
 *
 * Copyright (c) 2014 Carnegie Mellon University
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *******************************************************************************/

#ifndef IKOS_EXTENDED_CONSTRAINTS_HPP
#define IKOS_EXTENDED_CONSTRAINTS_HPP

#include <boost/optional.hpp>
#include <ikos/common.hpp>
#include <ikos/patricia_trees.hpp>
#include <ikos/collections.hpp>
#include <ikos/linear_constraints.hpp>

namespace ikos {

  template< typename Number, typename VariableName >
  class extended_constraint: public writeable {

  public:
    typedef extended_constraint< Number, VariableName > extended_constraint_t;
    typedef linear_constraint< Number, VariableName > linear_constraint_t;
    typedef variable< Number, VariableName > variable_t;
    typedef linear_expression< Number, VariableName > linear_expression_t;
    typedef patricia_tree_set< variable_t > variable_set_t;
    typedef enum {
      INF,
      INF_EQ,
      SUP,
      SUP_EQ,
      EQ,
      NOT_EQ,
      MOD
    } kind_t;
    typedef typename linear_expression_t::iterator iterator;

  private:
    kind_t _kind;
    linear_expression_t _expr;
    boost::optional<Number> _modulus;

  public:
    extended_constraint(): _kind(EQ) { }

    extended_constraint(linear_expression_t expr, kind_t kind, Number modulus): _kind(kind), _expr(expr), _modulus(modulus) { }

    extended_constraint(linear_expression_t expr, kind_t kind): _kind(kind), _expr(expr) { }

    extended_constraint(linear_constraint_t constraint): _kind(EQ), _expr(constraint.expression()) {
      if(constraint.is_inequality()) {
        _kind = INF_EQ;
      } else if(constraint.is_equality()) {
        _kind = EQ;
      } else {
        _kind = NOT_EQ;
      }
    }

    bool is_tautology() {
      switch (this->_kind) {
        case INF: {
          return (this->_expr.is_constant() && this->_expr.constant() < 0);
        }
        case INF_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() <= 0);
        }
        case SUP: {
          return (this->_expr.is_constant() && this->_expr.constant() > 0);
        }
        case SUP_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() >= 0);
        }
        case EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() == 0);
        }
        case NOT_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() != 0);
        }
        case MOD: {
          return false; // TODO: type dependent
        }
        default: {
          throw error("Unreachable");
        }
      }
    }

    bool is_contradiction() {
      switch (this->_kind) {
        case INF: {
          return (this->_expr.is_constant() && this->_expr.constant() >= 0);
        }
        case INF_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() > 0);
        }
        case SUP: {
          return (this->_expr.is_constant() && this->_expr.constant() <= 0);
        }
        case SUP_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() < 0);
        }
        case EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() != 0);
        }
        case NOT_EQ: {
          return (this->_expr.is_constant() && this->_expr.constant() == 0);
        }
        case MOD: {
          return false; // TODO: type dependent
        }
        default: {
          throw error("Unreachable");
        }
      }
    }

    linear_expression_t expression() {
      return this->_expr;
    }

    kind_t kind() {
      return this->_kind;
    }

    boost::optional<Number> modulus() {
      return this->_modulus;
    }

    iterator begin() {
      return this->_expr.begin();
    }

    iterator end() {
      return this->_expr.end();
    }

    Number constant() {
      return -this->_expr.constant();
    }

    std::size_t size() {
      return this->_expr.size();
    }

    Number operator[](variable_t x) {
      return this->_expr.operator[](x);
    }

    variable_set_t variables() {
      return this->_expr.variables();
    }

    std::ostream& write(std::ostream& o) {
      if (this->is_contradiction()) {
        o << "false";
      } else if (this->is_tautology()) {
        o << "true";
      } else {
        linear_expression_t e = this->_expr - this->_expr.constant();
        o << e;
        switch (this->_kind) {
          case INF: {
            o << " < ";
            break;
          }
          case INF_EQ: {
            o << " <= ";
            break;
          }
          case SUP: {
            o << " > ";
            break;
          }
          case SUP_EQ: {
            o << " >= ";
            break;
          }
          case EQ: {
            o << " = ";
            break;
          }
          case NOT_EQ: {
            o << " != ";
            break;
          }
          case MOD: {
            o << " = ";
            break;
          }
          default: {
            throw error("Unreachable");
          }
        }
        Number c = -this->_expr.constant();
        o << c;

        if (this->_kind == MOD) {
          o << " [";
          o << *_modulus;
          o << "]";
        }
      }
      return o;
    }

  }; // class extended_constraint

  template< typename Number, typename VariableName >
  class extended_constraint_system: public writeable {

  public:
    typedef extended_constraint< Number, VariableName > extended_constraint_t;
    typedef extended_constraint_system< Number, VariableName > extended_constraint_system_t;
    typedef linear_constraint< Number, VariableName > linear_constraint_t;
    typedef linear_constraint_system< Number, VariableName > linear_constraint_system_t;
    typedef variable< Number, VariableName > variable_t;
    typedef patricia_tree_set< variable_t > variable_set_t;

  private:
    typedef collection< extended_constraint_t > cst_collection_t;

  public:
    typedef typename cst_collection_t::iterator iterator;

  private:
    cst_collection_t _csts;

  public:
    extended_constraint_system() { }

    extended_constraint_system(extended_constraint_t cst) {
      this->_csts += cst;
    }

    extended_constraint_system_t& operator+=(extended_constraint_t cst) {
      this->_csts += cst;
      return *this;
    }

    extended_constraint_system_t& operator+=(extended_constraint_system_t s) {
      this->_csts += s._csts;
      return *this;
    }

    extended_constraint_system_t& operator+=(linear_constraint_t cst) {
      this->_csts += extended_constraint_t(cst);
      return *this;
    }

    extended_constraint_system_t& operator+=(linear_constraint_system_t csts) {
      for(typename linear_constraint_system_t::iterator it = csts.begin(); it != csts.end(); ++it) {
        this->_csts += extended_constraint_t(*it);
      }

      return *this;
    }

    extended_constraint_system_t operator+(extended_constraint_system_t s) {
      extended_constraint_system_t r;
      r.operator+=(s);
      r.operator+=(*this);
      return r;
    }

    iterator begin() {
      return this->_csts.begin();
    }

    iterator end() {
      return this->_csts.end();
    }

    variable_set_t variables() {
      variable_set_t variables;
      for (iterator it = this->begin(); it != this->end(); ++it) {
        variables |= it->variables();
      }
      return variables;
    }

    std::size_t size() {
      return this->_csts.size();
    }

    std::ostream& write(std::ostream& o) {
      return this->_csts.write(o);
    }

  }; // class extended_constraint_system
}

#endif // IKOS_EXTENDED_CONSTRAINTS_HPP
