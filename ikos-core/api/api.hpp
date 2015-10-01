/********************************************************************************
 *
 * Common IKOS API
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

#ifndef IKOS_API_HPP
#define IKOS_API_HPP

#include <ikos/common.hpp>
#include <ikos/bignums.hpp>
#include <ikos/linear_constraints.hpp>
#include <ikos/fwd_fixpoint_iterators.hpp>
#include <ikos/muzq.hpp>
#include <api/extended_constraints.hpp>
#include <api/domain_constraints.hpp>

/* abstract domains */
#include <ikos/constants.hpp>
#include <ikos/octagons.hpp>
#include <ikos/domain_products.hpp>

namespace ikos {

  namespace api {

    typedef string_factory::indexed_string VarName;
    typedef std::string CheckPointName;

    typedef variable< z_number, VarName > z_var_t;
    typedef variable< q_number, VarName > q_var_t;
    typedef linear_expression< z_number, VarName > z_linear_expression_t;
    typedef linear_expression< q_number, VarName > q_linear_expression_t;
    typedef linear_constraint< z_number, VarName > z_linear_constraint_t;
    typedef linear_constraint< q_number, VarName > q_linear_constraint_t;
    typedef extended_constraint< z_number, VarName > z_extended_constraint_t;
    typedef extended_constraint< q_number, VarName > q_extended_constraint_t;
    typedef extended_constraint_system< z_number, VarName > z_extended_constraint_system_t;
    typedef extended_constraint_system< q_number, VarName > q_extended_constraint_system_t;

    /* statements */
    typedef muzq::statement< VarName, CheckPointName > statement_t;
    typedef muzq::z_binary_operation< VarName, CheckPointName > z_binary_operation_t;
    typedef muzq::z_linear_assignment< VarName, CheckPointName > z_linear_assignment_t;
    typedef muzq::z_linear_assertion< VarName, CheckPointName > z_linear_assertion_t;
    typedef muzq::q_binary_operation< VarName, CheckPointName > q_binary_operation_t;
    typedef muzq::q_linear_assignment< VarName, CheckPointName > q_linear_assignment_t;
    typedef muzq::q_linear_assertion< VarName, CheckPointName > q_linear_assertion_t;
    typedef muzq::checkpoint< VarName, CheckPointName > checkpoint_t;

    /* cfg */
    typedef muzq::basic_block< VarName, CheckPointName > block_t;
    typedef block_t::identifier_t nodename_t;
    typedef muzq::muzq_cfg< VarName, CheckPointName > cfg_t;

    /* Fix point iterator */

    template< typename AbstractValue >
    class statement_analyzer: public muzq::statement_visitor< VarName, CheckPointName > {
      private:
        AbstractValue _domain;
        std::map< CheckPointName, AbstractValue >& _checkpoints;

      public:
        statement_analyzer(AbstractValue domain, std::map< CheckPointName, AbstractValue >& checkpoints): _domain(domain), _checkpoints(checkpoints) { }

        AbstractValue domain() {
          return this->_domain;
        }

        void visit(z_binary_operation_t& stmt) {
          this->_domain.first().apply(stmt.operation(), stmt.lhs().name(), stmt.left_operand().name(), stmt.right_operand().name());
        }

        void visit(z_linear_assignment_t& stmt) {
          this->_domain.first().assign(stmt.lhs().name(), stmt.rhs());
        }

        void visit(z_linear_assertion_t& stmt) {
          this->_domain.first() += stmt.constraint();
        }

        void visit(q_binary_operation_t& stmt) {
          this->_domain.second().apply(stmt.operation(), stmt.lhs().name(), stmt.left_operand().name(), stmt.right_operand().name());
        }

        void visit(q_linear_assignment_t& stmt) {
          this->_domain.second().assign(stmt.lhs().name(), stmt.rhs());
        }

        void visit(q_linear_assertion_t& stmt) {
          this->_domain.second() += stmt.constraint();
        }

        void visit(checkpoint_t& stmt) {
          this->_checkpoints[stmt.name()] = this->_domain;
        }

    }; // class statement_analyzer

    template< typename ZAbstractValue, typename QAbstractValue >
    class fixpoint_iterator :
        public interleaved_fwd_fixpoint_iterator< nodename_t, cfg_t, domain_product2< ZAbstractValue, QAbstractValue > > {
      public:
        typedef domain_product2< ZAbstractValue, QAbstractValue > AbstractValue;

      private:
        std::map< CheckPointName, AbstractValue > _checkpoints;

      public:
        fixpoint_iterator(cfg_t cfg) :
          interleaved_fwd_fixpoint_iterator< nodename_t, cfg_t, AbstractValue >(cfg) { }

        std::map< CheckPointName, AbstractValue >& checkpoints() {
          return _checkpoints;
        }

        void run_from_top() {
          this->run(AbstractValue(ZAbstractValue::top(), QAbstractValue::top()));
        }

        AbstractValue analyze(nodename_t node_name, AbstractValue pre) {
          block_t& block = this->get_cfg().get_node(node_name);
          statement_analyzer< AbstractValue > analyzer(pre, _checkpoints);

          for(block_t::iterator it = block.begin(); it != block.end(); ++it) {
            it->accept(&analyzer);
          }

          return analyzer.domain();
        }

        void process_pre(nodename_t node_name, AbstractValue inv) { }

        void process_post(nodename_t node_name, AbstractValue inv) { }

    }; // class fixpoint_iterator

  } // namespace api

} // namespace ikos

#endif // IKOS_API_HPP
