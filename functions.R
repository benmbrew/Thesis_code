library(Hmisc)
library(MASS)

#####
# FUNCTION FOR PRETTY BARPLOTS
#####
bar_fun <- function(var,
                    names.arg = NA,
                    xlab = NA,
                    ylab = NA,
                    col = adjustcolor("lightblue", alpha.f = 0.6),
                    border = NA,
                    cex.names = 0.76,
                    add_text = TRUE,
                    text_size = 0.75,
                    round_digits = 1,
                    percent = TRUE,
                    text_col = adjustcolor("black", alpha.f = 0.7),
                    grid_sequence = seq(0,100,20),
                    add_box = TRUE,
                    add_gridlines = TRUE,
                    add_ci = FALSE,
                    yplus = NULL,
                    yminus = NULL,
                    add = FALSE){
  # Draw plot
  bp <- barplot(var,
                names.arg = names.arg,
                ylab = ylab,
                xlab = xlab,
                col = col,
                border = border,
                ylim = c(0, max(var) * 1.1),
                cex.names = cex.names,
                add = add)
  # Add text
  if(add_text){
    text(x = bp[,1],
         y = var,
         labels = paste0(round(var, digits = round_digits),ifelse(percent, " %", "")),
         pos = 1,
         col = text_col,
         cex = text_size)
  }
  
  # Add gride lines
  if(add_gridlines){
    abline(h = grid_sequence,
           col = adjustcolor("black", alpha.f = 0.1),
           lty = 6)
  }
  # Add box
  if(add_box){
    box("plot")
  }
  
  # Add CI
  if(add_ci){
    if(is.null(yplus)|is.null(yminus)){
      stop("You must supply yplus and yminus values for confidence interval")
    } else {
      x_pos <- ifelse(add, -0.1, 0.1)
      errbar(x = bp[,1] + x_pos,
             y = var,
             yplus = yplus,
             yminus = yminus,
             add = TRUE,
             pch = NA,
             errbar.col = adjustcolor("darkred", alpha.f = 0.3))
    }
  }
}


#####
# FUNCTION FOR COMPUTING CONFIDENCE INTERVALS ON PROPORTIONS
# https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/
#####

simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

#####
# FUNCTION FOR IVR
# From: https://diffuseprior.wordpress.com/2012/05/03/an-ivreg2-function-for-r/
#####

ivreg2 <- function(form,endog,iv,data,digits=3){
  # model setup
  r1 <- lm(form,data)
  y <- r1$fitted.values+r1$resid
  x <- model.matrix(r1)
  aa <- rbind(endog == colnames(x),1:dim(x)[2])  
  z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv])  
  colnames(z)[(dim(z)[2]-length(iv)+1):(dim(z)[2])] <- iv  
  # iv coefficients and standard errors
  z <- as.matrix(z)
  pz <- z %*% (solve(crossprod(z))) %*% t(z)
  biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
  sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
  vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
  res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
  res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res) <- colnames(x)
  colnames(res) <- c("Coef","S.E.","t-stat","p-val")
  # First-stage F-test
  y1 <- data[,endog]
  z1 <- x[,aa[2,aa[1,]==0]]
  bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
  bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
  rss1 <- sum((y1 - z %*% bet1)^2)
  rss2 <- sum((y1 - z1 %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  firststage <- c(fs)
  firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
  colnames(firststage) <- c("First Stage F-test")
  # Hausman tests
  bols <- solve(crossprod(x)) %*% crossprod(x,y) 
  sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
  vbols <- as.numeric(sigols)*solve(crossprod(x))
  sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
  x1 <- x[,!(colnames(x) %in% "(Intercept)")]
  z1 <- z[,!(colnames(z) %in% "(Intercept)")]
  pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
  biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
  bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
  # Durbin-Wu-Hausman chi-sq test:
  # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
  # hpvl <- 1-pchisq(haus,df=1)
  # Wu-Hausman F test
  resids <- NULL
  resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
  x2 <- cbind(x,resids)
  bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
  bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
  rss1 <- sum((y - x2 %*% bet1)^2)
  rss2 <- sum((y - x %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  fpval <- 1-pf(fs, p1-p2, n1-p1)
  #hawu <- c(haus,hpvl,fs,fpval)
  hawu <- c(fs,fpval)
  hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
  #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
  colnames(hawu) <- c("Wu-Hausman F-test","p-val")  
  # Sargan Over-id test
  ivres <- y - (x %*% biv)
  oid <- solve(crossprod(z)) %*% crossprod(z,ivres)
  sstot <- sum((ivres-mean(ivres))^2)
  sserr <- sum((ivres - (z %*% oid))^2)
  rsq <- 1-(sserr/sstot)
  sargan <- length(ivres)*rsq
  spval <- 1-pchisq(sargan,df=length(iv)-1)
  overid <- c(sargan,spval)
  overid <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),overid)),ncol=length(overid))
  colnames(overid) <- c("Sargan test of over-identifying restrictions","p-val")
  if(length(iv)-1==0){
    overid <- t(matrix(c("No test performed. Model is just identified")))
    colnames(overid) <- c("Sargan test of over-identifying restrictions")
  }
  full <- list(results=res, weakidtest=firststage, endogeneity=hawu, overid=overid)
  return(full)
}