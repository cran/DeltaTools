#' @noRd
generate.matched.cohort <- function(dat, ID, target, features, MDR, mm, md, ml, mdo, mes, mex,
                                    mah, ate, mdi, mre, msw, mrp, mord, wid, rat, verb, ino, dfi,
                                    radius.days){
  data0 <- dat
  names(data0)[names(data0) == ID] <- "id"
  data0$id<-as.character(data0$id)
  formular<-generate.formular.string(target, features)
  if (MDR==FALSE){
    pairs.id <- MatchIt::matchit(formular, data=data0, method=mm, distance=md, link=ml, distance.options=mdo,
                        estimand=mes, exact=mex, mahvars=mah, antiexact=ate, discard=mdi, reestimate=mre,
                        s.weights=msw, replace=mrp, m.order=mord, caliper=wid, std.caliper=FALSE, ratio=rat,
                        verbose=verb, include.obj=ino)
    if (mrp==TRUE){
      pairs <- MatchIt::get_matches(object=pairs.id, id="id_row", data=data0)
    }
    else if (mrp==FALSE){
      pairs <- MatchIt::match.data(object=pairs.id, data=data0)
    }
  }
  else if (MDR==TRUE){
    # Create subgroups within specified date range and match within subgroups
    names(data0)[names(data0) == dfi] <- "sequencer"
    data0$sequencer<-as.Date(data0$sequencer, format="%Y-%m-%d")
    data0$sequencer.lower<-data0$sequencer-radius.days
    data0$sequencer.upper<-data0$sequencer+radius.days
    list.dates <- unique(as.Date(data0$sequencer)[data0[,target]==1])
    pairs = data.frame()
    run.match <- function(i){
      tryCatch(
        expr = {
          select.treated <- data0[data0$sequencer==i & data0[,target]==1,]
          sequencer.lower <- min(select.treated$sequencer.lower[select.treated[,target]==1])
          sequencer.upper <- min(select.treated$sequencer.upper[select.treated[,target]==1])
          select.control <- data0[data0[,target] == 0 & data0$sequencer >= sequencer.lower & data0$sequencer <= sequencer.upper,]
          sub <- rbind(select.treated, select.control)
          pairs.id<-MatchIt::matchit(formular, data=sub, method=mm, distance=md, link=ml, distance.options=mdo,
                            estimand=mes, exact=mex, mahvars=mah, antiexact=ate, discard=mdi, reestimate=mre,
                            s.weights=msw, replace=mrp, m.order=mord, caliper=wid, std.caliper=FALSE,
                            ratio=rat, verbose=verb, include.obj=ino)
          return(list(status=TRUE, message='Success', sub=sub, pairs.id=pairs.id))
        },
        error = function(e){
          return(list(status=FALSE, message=e))
        },
        warning = function(w){
          return(list(status=FALSE, message=w))
        },
        finally = {
        }
      )
    }
    for (i in list.dates){
      lres <- run.match(i)
      if (lres$status==TRUE & mrp==TRUE){
        pairs.new <- MatchIt::get_matches(object=lres$pairs.id, id="id_row", data=lres$sub)
        pairs <- rbind(pairs, data.frame(pairs.new))
      }
      else if (lres$status==TRUE & mrp==FALSE){
        pairs.new <- MatchIt::match.data(object=lres$pairs.id, data=lres$sub)
        pairs <- rbind(pairs, data.frame(pairs.new))
        # If matching without replacement, remove matched control cases
        data0 <- data0[!(data0$id %in% pairs.new$id[pairs.new[,target] == 0]),]
      }
    }
  }
  if (mrp==TRUE){
    pairs <- subset(pairs, select = -c(id_row))
  }
  if (MDR==TRUE){
    pairs <- subset(pairs, select=-c(sequencer.upper, sequencer.lower))
    pairs<-pairs %>%
      dplyr::rename(!!paste(dfi) := sequencer)
  }
  if(exists("pairs") && !is.null(pairs)){
    return(list(pairs=pairs, message="Success"))
  }
  else{
    return(list(message="Matching failed"))
  }
}
