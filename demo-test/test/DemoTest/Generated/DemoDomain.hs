module DemoTest.Generated.DemoDomain where
import Test.Tasty (TestTree)
import Rulecheck.Testing (SomeTestableRule (..), TestableRule (..), testSomeTestableRule)
import qualified DemoDomain
fn_lhs_divzuid :: DemoDomain.Expr -> DemoDomain.Expr
fn_lhs_divzuid x_a847 = x_a847 DemoDomain../ x_a847
fn_rhs_divzuid :: DemoDomain.Expr -> DemoDomain.Expr
fn_rhs_divzuid x_a847 = DemoDomain.Const 1
rule_divzuid :: SomeTestableRule
rule_divzuid = SomeTestableRule (TestableRule fn_lhs_divzuid fn_rhs_divzuid)
test_divzuid :: TestTree
test_divzuid = testSomeTestableRule "divzuid" rule_divzuid
fn_lhs_mul1 :: DemoDomain.Expr -> DemoDomain.Expr
fn_lhs_mul1 x_a848 = x_a848 DemoDomain..* DemoDomain.Const 1
fn_rhs_mul1 :: DemoDomain.Expr -> DemoDomain.Expr
fn_rhs_mul1 x_a848 = x_a848
rule_mul1 :: SomeTestableRule
rule_mul1 = SomeTestableRule (TestableRule fn_lhs_mul1 fn_rhs_mul1)
test_mul1 :: TestTree
test_mul1 = testSomeTestableRule "mul1" rule_mul1