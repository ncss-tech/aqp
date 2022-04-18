# context("NCSP algorithm")
# 
# data("jacobs2000")
# x <- jacobs2000
# 
# test_that('error conditions', {
#   
#   # x must be a SPC
#   expect_error(
#     aqp:::.NCSP(3L, vars = c('sand', 'clay'))
#   )
#   
#   # vars must exist in site / horizon
#   expect_error(
#     aqp:::.NCSP(x, vars = c('sand', 'tacos'))
#   )
#   
#   # depthSequence must be reasonable
#   expect_error(
#     aqp:::.NCSP(x, vars = c('sand', 'clay'), maxDepth = -5)
#   )
#   
#   
# })
# 
# 
# ## clearly not done yet
# test_that('works as expected', {
#   
#   skip('not ready for this yet')
#   
#   m.ncsp <- aqp:::.NCSP(x, vars = c('sand', 'clay'))
#   dim(m.ncsp)
#   m.ncsp[, 3]
#   
#   
#   m.ncsp <- aqp:::.NCSP(y, vars = c('sand', 'clay'), maxDepth = 200)
#   dim(m.ncsp)
#   
#   y <- x
#   y$clay[1] <- NA
#   m.ncsp <- aqp:::.NCSP(y, vars = c('sand', 'clay'))
#   
#   dim(m.ncsp)
#   m.ncsp
#   
#   
#   
#   
#   # m.pc <- profile_compare(x, vars = c('sand', 'clay'), max_d = max(x), k = 0)
# 
#   # # same
#   # dimnames(m.pc) <- dimnames(m.ncsp)
#   # dim(m.ncsp)
#   # dim(m.pc)
#   # expect_true(all.equal(m.ncsp, m.pc))
#   # 
#   
#   
# })
