/********************************************************************************
 *
 * Constraints of an Abstract Domain
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

#ifndef IKOS_DOMAIN_CONSTRAINTS_HPP
#define IKOS_DOMAIN_CONSTRAINTS_HPP

#include <ikos/common.hpp>
#include <ikos/bignums.hpp>
#include <ikos/linear_constraints.hpp>
#include <api/extended_constraints.hpp>

/* domains */
#include <ikos/constants.hpp>
#include <ikos/octagons.hpp>

namespace ikos {

  template< typename Number, typename VariableName >
  class domain_constraints {

  public:
    typedef variable< Number, VariableName > variable_t;
    typedef linear_expression< Number, VariableName > linear_expression_t;
    typedef extended_constraint< Number, VariableName > extended_constraint_t;
    typedef extended_constraint_system< Number, VariableName > extended_constraint_system_t;

    typedef constant_domain< Number, VariableName > constant_domain_t;
    typedef interval_domain< Number, VariableName > interval_domain_t;
    typedef octagon< Number, VariableName > octagon_t;

  private:
    extended_constraint_system_t _get_constraints(constant_domain_t& domain) {
      extended_constraint_system_t csts;

      for(typename constant_domain_t::iterator it = domain.begin(); it != domain.end(); ++it) {
        if(it->second.is_number()) {
          csts += extended_constraint_t(variable_t(it->first) - it->second.number().get(), extended_constraint_t::EQ);
        }
      }

      return csts;
    }

    extended_constraint_system_t _get_constraints(interval_domain_t& domain) {
      extended_constraint_system_t csts;

      for(typename interval_domain_t::iterator it = domain.begin(); it != domain.end(); ++it) {
        if(it->second.singleton()) {
          csts += extended_constraint_t(variable_t(it->first) - it->second.singleton().get(), extended_constraint_t::EQ);
        }
        else {
          if(!it->second.lb().is_minus_infinity()) {
            csts += extended_constraint_t(variable_t(it->first) - it->second.lb().number().get(), extended_constraint_t::SUP_EQ);
          }

          if(!it->second.ub().is_plus_infinity()) {
            csts += extended_constraint_t(variable_t(it->first) - it->second.ub().number().get(), extended_constraint_t::INF_EQ);
          }
        }
      }

      return csts;
    }

    extended_constraint_system_t _get_constraints(octagon_t& domain) {
      extended_constraint_system_t csts;
      csts += domain.constraints();
      return csts;
    }

  public:
    template< typename AbstractDomain >
    extended_constraint_system_t get_constraints(AbstractDomain& domain) {
      if(domain.is_bottom()) {
        extended_constraint_system_t csts;

        // constraint (0 != 0)
        csts += extended_constraint_t(linear_expression_t(), extended_constraint_t::NOT_EQ);

        return csts;
      }
      else if(domain.is_top()) {
        return extended_constraint_system_t();
      }
      else {
        return this->_get_constraints(domain);
      }
    }

  };

}

#endif // IKOS_DOMAIN_CONSTRAINTS_HPP
