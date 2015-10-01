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

#include <api/api.hpp>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
}

using namespace std;
using namespace ikos;
using namespace ikos::api;

/*
 * Conversion class, from OCAML to IKOS
 */
class CFGFactory {
  private:
    string_factory V;

  public:
    z_var_t get_z_var(value ml_var) {
      CAMLparam1(ml_var);
      string name = String_val(ml_var);
      CAMLreturnT(z_var_t, z_var_t(V[name]));
    }

    q_var_t get_q_var(value ml_var) {
      CAMLparam1(ml_var);
      string name = String_val(ml_var);
      CAMLreturnT(q_var_t, q_var_t(V[name]));
    }

    z_linear_expression_t get_z_linear_expression(value ml_expr) {
      CAMLparam1(ml_expr);
      CAMLlocal1(ml_head);
      z_linear_expression_t ikos_expr;

      while(ml_expr != Val_emptylist) {
        ml_head = Field(ml_expr, 0);

        if(Tag_val(ml_head) == 0) /* C_ZConst */
          ikos_expr = ikos_expr + z_number(Int_val(Field(ml_head, 0)));
        else /* C_ZTerm */
          ikos_expr = ikos_expr + z_number(Int_val(Field(ml_head, 0))) * get_z_var(Field(ml_head, 1));

        ml_expr = Field(ml_expr, 1);
      }

      CAMLreturnT(z_linear_expression_t, ikos_expr);
    }

    q_linear_expression_t get_q_linear_expression(value ml_expr) {
      CAMLparam1(ml_expr);
      CAMLlocal1(ml_head);
      q_linear_expression_t ikos_expr;

      while(ml_expr != Val_emptylist) {
        ml_head = Field(ml_expr, 0);

        if(Tag_val(ml_head) == 0) /* C_QConst */
          ikos_expr = ikos_expr + q_number(Int_val(Field(ml_head, 0)), Int_val(Field(ml_head, 1)));
        else /* C_QTerm */
          ikos_expr = ikos_expr + q_number(Int_val(Field(ml_head, 0)), Int_val(Field(ml_head, 1))) * get_q_var(Field(ml_head, 2));

        ml_expr = Field(ml_expr, 1);
      }

      CAMLreturnT(q_linear_expression_t, ikos_expr);
    }

    z_linear_constraint_t get_z_linear_constraint(value ml_constraint) {
      CAMLparam1(ml_constraint);
      z_linear_expression_t ikos_expr = get_z_linear_expression(Field(ml_constraint, 0));
      z_linear_constraint_t::kind_t kind;

      switch(Int_val(Field(ml_constraint, 1))) {
        case 0: /* C_InfEq */
          kind = z_linear_constraint_t::INEQUALITY;
          break;
        case 1: /* C_Eq */
          kind = z_linear_constraint_t::EQUALITY;
          break;
        case 2: /* C_NotEq */
        default:
          kind = z_linear_constraint_t::DISEQUATION;
          break;
      }

      CAMLreturnT(z_linear_constraint_t, z_linear_constraint_t(ikos_expr, kind));
    }

    q_linear_constraint_t get_q_linear_constraint(value ml_constraint) {
      CAMLparam1(ml_constraint);
      q_linear_expression_t ikos_expr = get_q_linear_expression(Field(ml_constraint, 0));
      q_linear_constraint_t::kind_t kind;

      switch(Int_val(Field(ml_constraint, 1))) {
        case 0: /* C_InfEq */
          kind = q_linear_constraint_t::INEQUALITY;
          break;
        case 1: /* C_Eq */
          kind = q_linear_constraint_t::EQUALITY;
          break;
        case 2: /* C_NotEq */
        default:
          kind = q_linear_constraint_t::DISEQUATION;
          break;
      }

      CAMLreturnT(q_linear_constraint_t, q_linear_constraint_t(ikos_expr, kind));
    }

    cfg_t get_cfg(value ml_cfg) {
      CAMLparam1(ml_cfg);
      CAMLlocal4(ml_blocks, ml_block, ml_links, ml_link);
      cfg_t cfg(String_val(Field(ml_cfg, 0)));

      /* create blocks */
      ml_blocks = Field(ml_cfg, 1);

      while(ml_blocks != Val_emptylist) {
        ml_block = Field(ml_blocks, 0);

        string name = String_val(Field(ml_block, 0));
        block_t& ikos_block = cfg.insert_basic_block(name);
        block_add_statements(ikos_block, Field(ml_block, 1));

        ml_blocks = Field(ml_blocks, 1);
      }

      /* create links */
      ml_links = Field(ml_cfg, 2);

      while(ml_links != Val_emptylist) {
        ml_link = Field(ml_links, 0);

        string from = String_val(Field(ml_link, 0));
        string to = String_val(Field(ml_link, 1));
        cfg.get_node(nodename_t(from)) >> cfg.get_node(nodename_t(to));

        ml_links = Field(ml_links, 1);
      }

      CAMLreturnT(cfg_t, cfg);
    }

  private:
    void block_add_statements(block_t& ikos_block, value ml_statements) {
      CAMLparam1(ml_statements);
      CAMLlocal1(ml_statement);

      while(ml_statements != Val_emptylist) {
        ml_statement = Field(ml_statements, 0);

        switch (Tag_val(ml_statement)) {
          case 0: /* C_ZAdd */
            ikos_block.add(get_z_var(Field(ml_statement, 0)),
                get_z_var(Field(ml_statement, 1)),
                get_z_var(Field(ml_statement, 2)));
            break;
          case 1: /* C_ZSub */
            ikos_block.sub(get_z_var(Field(ml_statement, 0)),
                get_z_var(Field(ml_statement, 1)),
                get_z_var(Field(ml_statement, 2)));
            break;
          case 2: /* C_ZMul */
            ikos_block.mul(get_z_var(Field(ml_statement, 0)),
                get_z_var(Field(ml_statement, 1)),
                get_z_var(Field(ml_statement, 2)));
            break;
          case 3: /* C_ZDiv */
            ikos_block.div(get_z_var(Field(ml_statement, 0)),
                get_z_var(Field(ml_statement, 1)),
                get_z_var(Field(ml_statement, 2)));
            break;
          case 4: /* C_QAdd */
            ikos_block.add(get_q_var(Field(ml_statement, 0)),
                get_q_var(Field(ml_statement, 1)),
                get_q_var(Field(ml_statement, 2)));
            break;
          case 5: /* C_QSub */
            ikos_block.sub(get_q_var(Field(ml_statement, 0)),
                get_q_var(Field(ml_statement, 1)),
                get_q_var(Field(ml_statement, 2)));
            break;
          case 6: /* C_QMul */
            ikos_block.mul(get_q_var(Field(ml_statement, 0)),
                get_q_var(Field(ml_statement, 1)),
                get_q_var(Field(ml_statement, 2)));
            break;
          case 7: /* C_QDiv */
            ikos_block.div(get_q_var(Field(ml_statement, 0)),
                get_q_var(Field(ml_statement, 1)),
                get_q_var(Field(ml_statement, 2)));
            break;
          case 8: /* C_ZAssign */
            ikos_block.assign(get_z_var(Field(ml_statement, 0)),
                get_z_linear_expression(Field(ml_statement, 1)));
            break;
          case 9: /* C_QAssign */
            ikos_block.assign(get_q_var(Field(ml_statement, 0)),
                get_q_linear_expression(Field(ml_statement, 1)));
            break;
          case 10: /* C_ZAssert */
            ikos_block.assertion(get_z_linear_constraint(Field(ml_statement, 0)));
            break;
          case 11: /* C_QAssert */
            ikos_block.assertion(get_q_linear_constraint(Field(ml_statement, 0)));
            break;
          case 12: /* C_Checkpoint */
            ikos_block.check(String_val(Field(ml_statement, 0)));
            break;
          default:
            throw error("unreachable");
        }

        ml_statements = Field(ml_statements, 1);
      }

      CAMLreturn0;
    }

}; // class CFGFactory

/*
 * Conversion class, from IKOS to OCAML
 */
class OcamlFactory {
  public:
    value get_z_linear_expression(z_linear_expression_t expr) {
      CAMLparam0();
      CAMLlocal3(result, item, term);
      result = Val_emptylist;

      for(z_linear_expression_t::iterator it = expr.begin(); it != expr.end(); ++it) {
        item = caml_alloc(2, 0);

        term = caml_alloc(2, 1);
        Store_field(term, 0, Val_int(it->first.si()));
        Store_field(term, 1, caml_copy_string(it->second.name().str().c_str()));

        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      if(expr.constant() != 0) {
        item = caml_alloc(2, 0);

        term = caml_alloc(1, 0);
        Store_field(term, 0, Val_int(expr.constant().si()));

        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      CAMLreturn(result);
    }

    value get_q_linear_expression(q_linear_expression_t expr) {
      CAMLparam0();
      CAMLlocal3(result, item, term);
      result = Val_emptylist;

      for(q_linear_expression_t::iterator it = expr.begin(); it != expr.end(); ++it) {
        item = caml_alloc(2, 0);

        term = caml_alloc(3, 1);
        Store_field(term, 0, Val_int(it->first.numerator().si()));
        Store_field(term, 1, Val_int(it->first.denominator().si()));
        Store_field(term, 2, caml_copy_string(it->second.name().str().c_str()));

        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      if(expr.constant() != 0) {
        item = caml_alloc(2, 0);

        term = caml_alloc(2, 0);
        Store_field(term, 0, Val_int(expr.constant().numerator().si()));
        Store_field(term, 1, Val_int(expr.constant().denominator().si()));

        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      CAMLreturn(result);
    }

    value get_z_linear_constraint(z_extended_constraint_t cst) {
      CAMLparam0();
      CAMLlocal3(result, expr_ml, op_ml);

      expr_ml = get_z_linear_expression(cst.expression());

      switch(cst.kind()) {
        case z_extended_constraint_t::INF:
          op_ml = Val_int(0); break;
        case z_extended_constraint_t::INF_EQ:
          op_ml = Val_int(1); break;
        case z_extended_constraint_t::SUP:
          op_ml = Val_int(2); break;
        case z_extended_constraint_t::SUP_EQ:
          op_ml = Val_int(3); break;
        case z_extended_constraint_t::EQ:
          op_ml = Val_int(4); break;
        case z_extended_constraint_t::NOT_EQ:
          op_ml = Val_int(5); break;
        case z_extended_constraint_t::MOD:
          op_ml = caml_alloc(1, 0);
          Store_field(op_ml, 0, Val_int(cst.modulus().get().si()));
          break;
        default:
          throw error("unreachable");
      }

      result = caml_alloc(2, 0);
      Store_field(result, 0, expr_ml);
      Store_field(result, 1, op_ml);

      CAMLreturn(result);
    }

    value get_q_linear_constraint(q_extended_constraint_t cst) {
      CAMLparam0();
      CAMLlocal3(result, expr_ml, op_ml);

      expr_ml = get_q_linear_expression(cst.expression());

      switch(cst.kind()) {
        case q_extended_constraint_t::INF:
          op_ml = Val_int(0); break;
        case q_extended_constraint_t::INF_EQ:
          op_ml = Val_int(1); break;
        case q_extended_constraint_t::SUP:
          op_ml = Val_int(2); break;
        case q_extended_constraint_t::SUP_EQ:
          op_ml = Val_int(3); break;
        case q_extended_constraint_t::EQ:
          op_ml = Val_int(4); break;
        case q_extended_constraint_t::NOT_EQ:
          op_ml = Val_int(5); break;
        default:
          throw error("unreachable");
      }

      result = caml_alloc(2, 0);
      Store_field(result, 0, expr_ml);
      Store_field(result, 1, op_ml);

      CAMLreturn(result);
    }

    value get_z_linear_constraints(z_extended_constraint_system_t csts) {
      CAMLparam0();
      CAMLlocal3(result, item, term);
      result = Val_emptylist;

      for(z_extended_constraint_system_t::iterator it = csts.begin(); it != csts.end(); ++it) {
        item = caml_alloc(2, 0);

        term = get_z_linear_constraint(*it);
        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      CAMLreturn(result);
    }

    value get_q_linear_constraints(q_extended_constraint_system_t csts) {
      CAMLparam0();
      CAMLlocal3(result, item, term);
      result = Val_emptylist;

      for(q_extended_constraint_system_t::iterator it = csts.begin(); it != csts.end(); ++it) {
        item = caml_alloc(2, 0);

        term = get_q_linear_constraint(*it);
        Store_field(item, 0, term);
        Store_field(item, 1, result);
        result = item;
      }

      CAMLreturn(result);
    }

}; // class OcamlFactory

/*
 * Fix point computation
 */
template< typename ZDomain, typename QDomain>
value compute_fixpoint_call(cfg_t& cfg) {
  typedef typename fixpoint_iterator< ZDomain, QDomain >::AbstractValue AbstractValue;
  CAMLparam0();
  CAMLlocal4(result, constraints, z_ml_csts, q_ml_csts);

  // tools
  OcamlFactory ml_factory;
  domain_constraints< z_number, VarName > z_domain_export;
  domain_constraints< q_number, VarName > q_domain_export;

  // init : create a Hashtbl
  result = caml_callback(*caml_named_value("hashtbl_create"), Val_int(10));

  fixpoint_iterator< ZDomain, QDomain > it(cfg);
  it.run_from_top();

  map< CheckPointName, AbstractValue >& checkpoints = it.checkpoints();
  for(typename map< CheckPointName, AbstractValue >::iterator it = checkpoints.begin(); it != checkpoints.end(); ++it) {
    CheckPointName name = it->first;

    z_extended_constraint_system_t z_csts = z_domain_export.get_constraints(it->second.first());
    q_extended_constraint_system_t q_csts = q_domain_export.get_constraints(it->second.second());

    z_ml_csts = ml_factory.get_z_linear_constraints(z_csts);
    q_ml_csts = ml_factory.get_q_linear_constraints(q_csts);

    constraints = caml_alloc(2, 0);
    Store_field(constraints, 0, z_ml_csts);
    Store_field(constraints, 1, q_ml_csts);

    // insert constraints in the Hashtbl
    caml_callback3(*caml_named_value("hashtbl_add"), result, caml_copy_string(name.c_str()), constraints);
  }

  CAMLreturn(result);
}

extern "C" {

/*
 * Binding functions
 */

value string_of_z_linear_expression(value ml_expr) {
  CAMLparam1(ml_expr);
  stringstream buffer;
  z_linear_expression_t e = CFGFactory().get_z_linear_expression(ml_expr);
  buffer << e;
  CAMLreturn(caml_copy_string(buffer.str().c_str()));
}

value string_of_q_linear_expression(value ml_expr) {
  CAMLparam1(ml_expr);
  stringstream buffer;
  q_linear_expression_t e = CFGFactory().get_q_linear_expression(ml_expr);
  buffer << e;
  CAMLreturn(caml_copy_string(buffer.str().c_str()));
}

value string_of_z_linear_constraint(value ml_constraint) {
  CAMLparam1(ml_constraint);
  stringstream buffer;
  z_linear_constraint_t c = CFGFactory().get_z_linear_constraint(ml_constraint);
  buffer << c;
  CAMLreturn(caml_copy_string(buffer.str().c_str()));
}

value string_of_q_linear_constraint(value ml_constraint) {
  CAMLparam1(ml_constraint);
  stringstream buffer;
  q_linear_constraint_t c = CFGFactory().get_q_linear_constraint(ml_constraint);
  buffer << c;
  CAMLreturn(caml_copy_string(buffer.str().c_str()));
}

value string_of_cfg(value ml_cfg) {
  CAMLparam1(ml_cfg);
  stringstream buffer;
  cfg_t cfg = CFGFactory().get_cfg(ml_cfg);
  buffer << cfg;
  CAMLreturn(caml_copy_string(buffer.str().c_str()));
}

/* it's ugly, but there is no other way */

#define ZDOMAIN_FROM_OCAML(var, type_name, todo)                         \
  do {                                                                   \
    if((var) == 0) {                                                     \
      typedef constant_domain< z_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == 1) {                                              \
      typedef interval_domain< z_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == 2) {                                              \
      typedef octagon< z_number, VarName > type_name;                    \
      todo;                                                              \
    }                                                                    \
  } while (0)

#define QDOMAIN_FROM_OCAML(var, type_name, todo)                         \
  do {                                                                   \
    if((var) == 0) {                                                     \
      typedef constant_domain< q_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == 1) {                                              \
      typedef interval_domain< q_number, VarName > type_name;            \
      todo;                                                              \
    } else if((var) == 2) {                                              \
      typedef octagon< q_number, VarName > type_name;                    \
      todo;                                                              \
    } else {                                                             \
      throw ikos::error("cannot use this domain with rational numbers"); \
    }                                                                    \
  } while (0)

value compute_fixpoint(value ml_z_domain, value ml_q_domain, value ml_cfg) {
  CAMLparam3(ml_z_domain, ml_q_domain, ml_cfg);

  cfg_t cfg = CFGFactory().get_cfg(ml_cfg);

  int z_tag = Int_val(ml_z_domain);
  int q_tag = Int_val(ml_q_domain);

  try {
    #define FIXPOINT_CALL CAMLreturn((compute_fixpoint_call< ZDomain, QDomain >(cfg)))
    #define FIXPOINT_CALL_QDOMAIN QDOMAIN_FROM_OCAML(q_tag, QDomain, FIXPOINT_CALL)
    ZDOMAIN_FROM_OCAML(z_tag, ZDomain, FIXPOINT_CALL_QDOMAIN);
    #undef FIXPOINT_CALL_QDOMAIN
    #undef FIXPOINT_CALL
  }
  catch(ikos::error e) {
    caml_raise_with_string(*caml_named_value("ikos_exception"), e.message().c_str());
  }

  CAMLreturn(Val_unit);
}

} // extern "C"
