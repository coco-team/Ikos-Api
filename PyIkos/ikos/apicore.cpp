/********************************************************************************
 *
 * Stub for the low-level API
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

#include <boost/python.hpp>
#include <boost/python/stl_iterator.hpp>

#include <api/api.hpp>

using namespace boost::python;
namespace python = boost::python;
using namespace ikos;
using namespace ikos::api;

/* imported class */
object fraction;

/*
 * Conversion class, from Python to IKOS
 */
class CFGFactory {
  private:
    string_factory V;

  public:
    z_var_t get_z_var(object py_var) {
      std::string name = extract< std::string >(py_var.attr("name"));
      return z_var_t(V[name]);
    }

    q_var_t get_q_var(object py_var) {
      std::string name = extract< std::string >(py_var.attr("name"));
      return q_var_t(V[name]);
    }

    z_linear_expression_t get_z_linear_expression(object py_expr) {
      z_linear_expression_t ikos_expr;

      object py_tuple = py_expr.attr("normalize")();
      long cst = extract< long >(py_tuple[1]);

      stl_input_iterator< object > it(py_tuple[0]), end;
      for(; it != end; ++it) {
        object py_term = *it;
        long factor = extract< long >(py_term[0]);
        ikos_expr = ikos_expr + z_number(factor) * get_z_var(py_term[1]);
      }

      ikos_expr = ikos_expr + z_number(cst);
      return ikos_expr;
    }

    q_linear_expression_t get_q_linear_expression(object py_expr) {
      q_linear_expression_t ikos_expr;

      object py_tuple = py_expr.attr("normalize")();
      long cst_num = extract< long >(py_tuple[1].attr("numerator"));
      long cst_den = extract< long >(py_tuple[1].attr("denominator"));


      stl_input_iterator< object > it(py_tuple[0]), end;
      for(; it != end; ++it) {
        object py_term = *it;
        long factor_num = extract< long >(py_term[0].attr("numerator"));
        long factor_den = extract< long >(py_term[0].attr("denominator"));
        ikos_expr = ikos_expr + q_number(factor_num, factor_den) * get_q_var(py_term[1]);
      }

      ikos_expr = ikos_expr + q_number(cst_num, cst_den);
      return ikos_expr;
    }

    z_linear_constraint_t get_z_linear_constraint(object py_constraint) {
      z_linear_expression_t ikos_expr = get_z_linear_expression(py_constraint.attr("expression"));
      z_linear_constraint_t::kind_t kind = z_linear_constraint_t::DISEQUATION;

      if(py_constraint.attr("operator") == "<=") {
        kind = z_linear_constraint_t::INEQUALITY;
      } else if(py_constraint.attr("operator") == "=") {
        kind = z_linear_constraint_t::EQUALITY;
      } else if(py_constraint.attr("operator") == "!=") {
        kind = z_linear_constraint_t::DISEQUATION;
      } else {
        throw_error("invalid operator in LinearConstraint: %s", py_constraint.attr("operator"));
      }

      return z_linear_constraint_t(ikos_expr, kind);
    }

    q_linear_constraint_t get_q_linear_constraint(object py_constraint) {
      q_linear_expression_t ikos_expr = get_q_linear_expression(py_constraint.attr("expression"));
      q_linear_constraint_t::kind_t kind = q_linear_constraint_t::DISEQUATION;

      if(py_constraint.attr("operator") == "<=") {
        kind = q_linear_constraint_t::INEQUALITY;
      } else if(py_constraint.attr("operator") == "=") {
        kind = q_linear_constraint_t::EQUALITY;
      } else if(py_constraint.attr("operator") == "!=") {
        kind = q_linear_constraint_t::DISEQUATION;
      } else {
        throw_error("invalid operator in LinearConstraint: %s", py_constraint.attr("operator"));
      }

      return q_linear_constraint_t(ikos_expr, kind);
    }

    cfg_t get_cfg(object py_cfg) {
      stl_input_iterator< object > end;
      std::string entry_name = extract< std::string >(py_cfg.attr("entry").attr("name"));
      cfg_t cfg(entry_name);

      /* create blocks */
      for(stl_input_iterator< object > it(py_cfg.attr("blocks")); it != end; ++it) {
        object py_block = *it;
        std::string block_name = extract< std::string >(py_block.attr("name"));

        block_t& block = cfg.insert_basic_block(block_name);
        block_add_statements(block, py_block.attr("statements"));
      }

      /* create links */
      for(stl_input_iterator< object > it(py_cfg.attr("blocks")); it != end; ++it) {
        object py_block = *it;
        std::string block_name = extract< std::string >(py_block.attr("name"));

        for(stl_input_iterator< object > it2(py_block.attr("next_blocks")); it2 != end; ++it2) {
          object py_next = *it2;
          std::string next_name = extract< std::string >(py_next.attr("name"));
          cfg.get_node(nodename_t(block_name)) >> cfg.get_node(nodename_t(next_name));
        }
      }

      return cfg;
    }

  private:
    void block_add_statements(block_t& block, object py_statements) {
      stl_input_iterator< object > it(py_statements), end;

      for(; it != end; ++it) {
        object py_statement = *it;
        object class_name = py_statement.attr("__class__").attr("__name__");

        if(class_name == "BinaryOperation") {
          object type = py_statement.attr("var").attr("type");
          object op = py_statement.attr("operator");

          if(type == "int") {
            z_var_t var = get_z_var(py_statement.attr("var"));
            z_var_t left = get_z_var(py_statement.attr("left"));
            z_var_t right = get_z_var(py_statement.attr("right"));

            if(op == "+") block.add(var, left, right);
            else if(op == "+") block.sub(var, left, right);
            else if(op == "*") block.mul(var, left, right);
            else if(op == "/") block.div(var, left, right);
            else throw_error("invalid operator in BinaryStatement: %s", op);
          }
          else if(type == "rational") {
            q_var_t var = get_q_var(py_statement.attr("var"));
            q_var_t left = get_q_var(py_statement.attr("left"));
            q_var_t right = get_q_var(py_statement.attr("right"));

            if(op == "+") block.add(var, left, right);
            else if(op == "+") block.sub(var, left, right);
            else if(op == "*") block.mul(var, left, right);
            else if(op == "/") block.div(var, left, right);
            else throw_error("invalid operator in BinaryStatement: %s", op);
          }
          else {
            throw_error("invalid variable type: %s", type);
          }
        }
        else if(class_name == "Assign") {
          object type = py_statement.attr("var").attr("type");

          if(type == "int") {
            z_var_t var = get_z_var(py_statement.attr("var"));
            z_linear_expression_t expr = get_z_linear_expression(py_statement.attr("expression"));
            block.assign(var, expr);
          }
          else if(type == "rational") {
            q_var_t var = get_q_var(py_statement.attr("var"));
            q_linear_expression_t expr = get_q_linear_expression(py_statement.attr("expression"));
            block.assign(var, expr);
          }
          else {
            throw_error("invalid variable type: %s", type);
          }
        }
        else if(class_name == "Assert") {
          object type = py_statement.attr("constraint").attr("type");

          if(type == "int") {
            z_linear_constraint_t cst = get_z_linear_constraint(py_statement.attr("constraint"));
            block.assertion(cst);
          }
          else if(type == "rational") {
            q_linear_constraint_t cst = get_q_linear_constraint(py_statement.attr("constraint"));
            block.assertion(cst);
          }
          else {
            throw_error("invalid LinearConstraint type: %s", type);
          }
        }
        else if(class_name == "Checkpoint") {
          std::string name = extract< std::string >(py_statement.attr("name"));
          block.check(name);
        }
        else {
          throw_error("invalid statement: %s", class_name);
        }
      }
    }

    void throw_error(std::string format, object value) {
      object py_message = format % value.attr("__repr__")();
      std::string message = extract< std::string >(py_message);
      throw ikos::error(message);
    }

}; // class CFGFactory


/*
 * Conversion class, from IKOS to Python
 */
class PythonFactory {
  public:
    python::list get_z_linear_expression(z_linear_expression_t expr) {
      python::list result;

      for(z_linear_expression_t::iterator it = expr.begin(); it != expr.end(); ++it) {
        result.append(python::make_tuple(it->first.si(), it->second.name().str()));
      }

      if(expr.constant() != 0) {
        result.append(expr.constant().si());
      }

      return result;
    }

    python::list get_q_linear_expression(q_linear_expression_t expr) {
      python::list result;

      for(q_linear_expression_t::iterator it = expr.begin(); it != expr.end(); ++it) {
        object factor = fraction(it->first.numerator().si(), it->first.denominator().si());
        result.append(python::make_tuple(factor, it->second.name().str()));
      }

      if(expr.constant() != 0) {
        object py_cst = fraction(expr.constant().numerator().si(), expr.constant().denominator().si());
        result.append(py_cst);
      }

      return result;
    }

    python::tuple get_z_linear_constraint(z_extended_constraint_t cst) {
      python::list py_expr = get_z_linear_expression(cst.expression());

      switch(cst.kind()) {
        case z_extended_constraint_t::INF:
          return python::make_tuple(py_expr, "<");
        case z_extended_constraint_t::INF_EQ:
          return python::make_tuple(py_expr, "<=");
        case z_extended_constraint_t::SUP:
          return python::make_tuple(py_expr, ">");
        case z_extended_constraint_t::SUP_EQ:
          return python::make_tuple(py_expr, ">=");
        case z_extended_constraint_t::EQ:
          return python::make_tuple(py_expr, "=");
        case z_extended_constraint_t::NOT_EQ:
          return python::make_tuple(py_expr, "!=");
        case z_extended_constraint_t::MOD:
          return python::make_tuple(py_expr, "mod", cst.modulus().get().si());
        default:
          throw error("unreachable");
      }
    }

    python::tuple get_q_linear_constraint(q_extended_constraint_t cst) {
      python::list py_expr = get_q_linear_expression(cst.expression());

      switch(cst.kind()) {
        case q_extended_constraint_t::INF:
          return python::make_tuple(py_expr, "<");
        case q_extended_constraint_t::INF_EQ:
          return python::make_tuple(py_expr, "<=");
        case q_extended_constraint_t::SUP:
          return python::make_tuple(py_expr, ">");
        case q_extended_constraint_t::SUP_EQ:
          return python::make_tuple(py_expr, ">=");
        case q_extended_constraint_t::EQ:
          return python::make_tuple(py_expr, "=");
        case q_extended_constraint_t::NOT_EQ:
          return python::make_tuple(py_expr, "!=");
        default:
          throw error("unreachable");
      }
    }

    python::list get_z_linear_constraints(z_extended_constraint_system_t csts) {
      python::list result;

      for(z_extended_constraint_system_t::iterator it = csts.begin(); it != csts.end(); ++it) {
        result.append(get_z_linear_constraint(*it));
      }

      return result;
    }

    python::list get_q_linear_constraints(q_extended_constraint_system_t csts) {
      python::list result;

      for(q_extended_constraint_system_t::iterator it = csts.begin(); it != csts.end(); ++it) {
        result.append(get_q_linear_constraint(*it));
      }

      return result;
    }

}; // class PythonFactory

/*
 * Fix point computation
 */
template< typename ZDomain, typename QDomain>
dict compute_fixpoint_call(cfg_t& cfg) {
  typedef typename fixpoint_iterator< ZDomain, QDomain >::AbstractValue AbstractValue;

  PythonFactory py_factory;
  domain_constraints< z_number, VarName > z_domain_export;
  domain_constraints< q_number, VarName > q_domain_export;
  dict result;

  fixpoint_iterator< ZDomain, QDomain > it(cfg);
  it.run_from_top();

  std::map< CheckPointName, AbstractValue >& checkpoints = it.checkpoints();
  for(typename std::map< CheckPointName, AbstractValue >::iterator it = checkpoints.begin(); it != checkpoints.end(); ++it) {
    CheckPointName name = it->first;

    z_extended_constraint_system_t z_csts = z_domain_export.get_constraints(it->second.first());
    q_extended_constraint_system_t q_csts = q_domain_export.get_constraints(it->second.second());

    python::list py_z_csts = py_factory.get_z_linear_constraints(z_csts);
    python::list py_q_csts = py_factory.get_q_linear_constraints(q_csts);

    result[name] = python::make_tuple(py_z_csts, py_q_csts);
  }

  return result;
}

/*
 * Ikos Exceptions
 */

/* Python C-API, only to define a custom exception (impossible with boost::python) */
PyObject* create_exception_class(const char* name, PyObject* base_type = PyExc_Exception)
{
  std::string scope_name = extract< std::string >(scope().attr("__name__"));
  std::string qualified_name = scope_name + "." + name;

  PyObject* type_obj = PyErr_NewException(const_cast< char* >(qualified_name.c_str()), base_type, 0);

  if(!type_obj)
    throw_error_already_set();

  scope().attr(name) = handle<>(borrowed(type_obj));
  return type_obj;
}

PyObject* ikos_exception = NULL;

void ikos_exception_translator(ikos::error const& e) {
  /*
   * Bad design in IKOS:
   * boost::python enforces having a const& argument for exception translators,
   * and error::message() is not const, that's why I use a copy here.
   */
  ikos::error e_copy(e);
  PyErr_SetString(ikos_exception, e_copy.message().c_str());
}

/*
 * Binding functions
 */

void print_linear_expression(object py_expr) {
  if(py_expr.attr("type") == "int") {
    z_linear_expression_t expr = CFGFactory().get_z_linear_expression(py_expr);
    std::cout << expr << std::endl;
  }
  else {
    q_linear_expression_t expr = CFGFactory().get_q_linear_expression(py_expr);
    std::cout << expr << std::endl;
  }
}

void print_linear_constraint(object py_constraint) {
  if(py_constraint.attr("type") == "int") {
    z_linear_constraint_t cst = CFGFactory().get_z_linear_constraint(py_constraint);
    std::cout << cst << std::endl;
  }
  else {
    q_linear_constraint_t cst = CFGFactory().get_q_linear_constraint(py_constraint);
    std::cout << cst << std::endl;
  }
}

void print_cfg(object py_cfg) {
  cfg_t cfg = CFGFactory().get_cfg(py_cfg);
  std::cout << cfg << std::endl;
}

/* it's ugly, but there is no other way */

#define ZDOMAIN_FROM_PYTHON(var, type_name, todo)                        \
  do {                                                                   \
    if((var) == "constant") {                                            \
      typedef constant_domain< z_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == "interval") {                                     \
      typedef interval_domain< z_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == "octagon") {                                      \
      typedef octagon< z_number, VarName > type_name;                    \
      todo;                                                              \
    }                                                                    \
  } while (0)

#define QDOMAIN_FROM_PYTHON(var, type_name, todo)                        \
  do {                                                                   \
    if((var) == "constant") {                                            \
      typedef constant_domain< q_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == "interval") {                                     \
      typedef interval_domain< q_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == "octagon") {                                      \
      typedef octagon< q_number, VarName > type_name;                    \
      todo;                                                              \
    } else {                                                             \
      throw ikos::error("cannot use this domain with rational numbers"); \
    }                                                                    \
  } while (0)

dict compute_fixpoint(object py_cfg, std::string py_z_domain, std::string py_q_domain) {
  cfg_t cfg = CFGFactory().get_cfg(py_cfg);

  #define FIXPOINT_CALL return compute_fixpoint_call< ZDomain, QDomain >(cfg)
  #define FIXPOINT_CALL_QDOMAIN QDOMAIN_FROM_PYTHON(py_q_domain, QDomain, FIXPOINT_CALL)
  ZDOMAIN_FROM_PYTHON(py_z_domain, ZDomain, FIXPOINT_CALL_QDOMAIN);
  #undef FIXPOINT_CALL_QDOMAIN
  #undef FIXPOINT_CALL

  return dict(); // unreachable
}


BOOST_PYTHON_MODULE(apicore) {
  /* import */
  fraction = import("fractions").attr("Fraction");

  /* exception handling */
  ikos_exception = create_exception_class("IkosException");
  register_exception_translator< ikos::error >(ikos_exception_translator);

  /* for debugging purpose */
  def("print_linear_expression", print_linear_expression, "Print a LinearExpression");
  def("print_linear_constraint", print_linear_constraint, "Print a LinearConstraint");
  def("print_cfg", print_cfg, "Print a Cfg");

  /* IKOS call */
  def("compute_fixpoint", compute_fixpoint, "Compute a fix point");
}
