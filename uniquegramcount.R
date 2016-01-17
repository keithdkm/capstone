ngramcount<-cbind( unigrams[order(x),.(x,count)],
                   bicount =   bigrams[unigrams$x,.N,by = x ][order(x),N ] + bigrams [unigrams$x,.N,by = w ][order(w),N ],
                   tricount = trigrams[unigrams$x,.N,by = x ][order(x),N ] + trigrams[unigrams$x,.N,by = w ][order(w),N ] + trigrams[unigrams$x,.N,by = v ][order(v),N ]
                   )
