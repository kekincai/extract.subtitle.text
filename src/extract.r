extract <- function(dir="./srt/") {
    fns <- list.files("./srt/", full.names = FALSE)
    for(i in seq_along(fns)) {
        fn <- paste0(dir, fns[i])
        srt <- scan(fn, "")
        is.del <- grepl("^00:|-->", srt) | srt %in% as.character(seq(500))
        srt <- srt[!is.del]
        suffix <- "(ます|ました|ましたね|ましょう|です|ません|ますね|ですね)"
        is.tail <- grepl(paste0(suffix, "$"), srt)
        is.head <- c(TRUE, head(is.tail, -1))
        srt.ls <- split(srt, cumsum(is.head))
        
        srt <- sapply(srt.ls, function(txt) {
            #browser()
            res <- paste(txt, collapse = "、")
            paste0(res, "。")
            })
        
        out.fn <- paste0("./out/", gsub("srt$", "txt", fns[i]))
        write(srt, out.fn)
    }
}

txt <- extract()