module DemoTest.Generated.DemoDomain where
import qualified DemoDomain
import Rulecheck.Testing (SomeTestableRule (..), TestableRule (..), testSomeTestableRule)
import Test.Tasty (TestTree)
fn_lhs_divzuid :: DemoDomain.Expr -> DemoDomain.Expr
fn_lhs_divzuid x_a2Hm = x_a2Hm DemoDomain../ x_a2Hm
fn_rhs_divzuid :: DemoDomain.Expr -> DemoDomain.Expr
fn_rhs_divzuid x_a2Hm = DemoDomain.Const 1
rule_divzuid :: SomeTestableRule
rule_divzuid = SomeTestableRule (TestableRule fn_lhs_divzuid fn_rhs_divzuid)
test_divzuid :: TestTree
test_divzuid = testSomeTestableRule "divzuid" rule_divzuid
fn_lhs_mul1 :: DemoDomain.Expr -> DemoDomain.Expr
fn_lhs_mul1 x_a2Hn = x_a2Hn DemoDomain..* DemoDomain.Const 1
fn_rhs_mul1 :: DemoDomain.Expr -> DemoDomain.Expr
fn_rhs_mul1 x_a2Hn = x_a2Hn
rule_mul1 :: SomeTestableRule
rule_mul1 = SomeTestableRule (TestableRule fn_lhs_mul1 fn_rhs_mul1)
test_mul1 :: TestTree
test_mul1 = testSomeTestableRule "mul1" rule_mul1
