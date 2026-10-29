library(creditratingsystem)
source(system.file('examples/wholesale_sector_journey_review.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_journey_review.json',package='creditratingsystem'),simplifyVector=FALSE)
if(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')!='true')d$governance$cases<-Filter(function(c)c$product%in%c('SOVEREIGN_DEBT','PUBLIC_SERVICE_FINANCE'),d$governance$cases)
v<-run_wholesale_sector_journey_review(d)
stopifnot(v$lineage$decision$status=='PASS')
for(c in v$cases){stopifnot(c$replay$decision$status=='PASS',c$negative_replay$decision$status=='BLOCKED',c$rating_benchmark$applicability=='NOT_APPLICABLE');if(c$product=='SOVEREIGN_DEBT')stopifnot(c$original_target$event=='pre_transfer_sovereign_payment_default');if(!is.null(c$prediction_review)){q<-c$prediction_review;p<-q$raw_pd;stopifnot(abs(q$benchmark$metrics$by_model$raw$brier-(.1*(1-p)^2+.9*p*p))<1e-12,q$override$metrics$overridden_cases==0)}}

# Exercise the CSV/JSON boundary independently of host long-double support.
# These are the binary64 results of the noLD decimal reader for the same text.
stopifnot(.sector_source_number_equal(231.18684964560401, 231.18684964560396))
rounded <- d
for (i in seq_along(rounded$claims)) rounded$claims[[i]]$source_balance <- rounded$claims[[i]]$source_balance*(1+2*.Machine$double.eps)
for (i in seq_along(rounded$governance$cases)) rounded$governance$cases[[i]]$features[[1]][[1]] <- rounded$governance$cases[[i]]$features[[1]][[1]]+2*.Machine$double.eps
rounded_result <- run_wholesale_sector_journey_review(rounded)
stopifnot(rounded_result$lineage$decision$status=='PASS')
for (i in seq_along(v$cases)) stopifnot(identical(rounded_result$cases[[i]]$source_feature,v$cases[[i]]$source_feature),identical(rounded_result$cases[[i]]$source_balance,v$cases[[i]]$source_balance))

# Real source changes must still fail at the binding check, before scoring.
expect_binding_error <- function(data) {
 error <- tryCatch({run_wholesale_sector_journey_review(data); NULL},error=identity)
 stopifnot(inherits(error,'error'),grepl('Source must bind',conditionMessage(error),fixed=TRUE))
}
changed <- d
changed$governance$cases[[1]]$product <- 'INCORRECT_PRODUCT'
expect_binding_error(changed)
claim_index <- which(vapply(d$claims,function(claim)claim$case_id==d$governance$cases[[1]]$case_id,logical(1)))
for (delta in c(-0.01,0.01)) {
 changed <- d
 changed$claims[[claim_index]]$source_balance <- changed$claims[[claim_index]]$source_balance+delta
 expect_binding_error(changed)
 changed <- d
 changed$governance$cases[[1]]$features[[1]][[1]] <- changed$governance$cases[[1]]$features[[1]][[1]]+delta
 expect_binding_error(changed)
}
for (invalid in list(NA_real_,NaN,Inf,-Inf,NULL,'6',c(6,6))) {
 stopifnot(!.sector_source_number_equal(invalid,6),!.sector_source_number_equal(6,invalid))
}
