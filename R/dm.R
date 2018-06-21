example.data.mapping = function() {
  library(gtreeCore)
  project.dir = "D:/libraries/gtree/myproject"
  tg = get.tg(gameId="LureOfAuthorityReduced",project.dir = project.dir)
  vg = get.vg(gameId="LureOfAuthorityReduced",project.dir = project.dir)
  oco.df = tg$oco.df

  dat = foreign::read.dta("D:/libraries/gtree/source/LureOfAuthority/LureofAuthority.dta")
  colnames(dat)

  df = dat

  df = df %>% select(-(HIGH:PHIGH50)) %>%
    select(variant, everything())

  source = df

  dest.cols = c("variant", tg$lev.vars)
  dest = oco.df[,dest.cols]

  shinyDataMapping(source=source, dest=dest)
}

shinyDataMapping = function(source, dest,...,launch.browser = TRUE) {
  app = dataMappingApp(source, dest)
  viewApp(app,launch.browser = launch.browser,...)
}

dataMappingApp = function(source, dest) {
  restore.point("dataMappingApp")
  app = eventsApp()
  input.ui = make.input.ui(source, dest)
  code = make.start.mapping.code(source, dest)
  ui = fluidPage(
    h4("Mapping Code"),
    aceEditor("codeEdit",height="12em",value=code, mode="r", wordWrap = TRUE, tabSize="2", useSoftTabs = FALSE, autoCompleteList = list(source=colnames(source), dest=colnames(dest))),
    fluidRow(
      column(5, h5("Mapped Columns"),input.ui),
      column(7,h5("Source Data"), dataTableOutput("sourceDT"))
    )
  )
  app$ui = ui
  appInitHandler(function(...) {
    setDataTable("sourceDT",datatable(source,class="display compact"), server=TRUE, quoted=TRUE)
  })



  app
}

trunc.values.string = function(vals, max.char=20) {
  vals = sort(unique(vals))
  str = paste0(vals, collapse=", ")
  if (nchar(str)<=max.char) return(str)
  str = paste0("[",length(vals),"] ", substring(str, 1,max.char-5),"...")
  str
}

make.input.ui = function(source, dest) {
  restore.point("make.input.ui")
  cols = colnames(dest)

  choices = c(".empty", colnames(source))
  nchoices = unlist(lapply(colnames(source), function(scol) {
    paste0(scol, " (", trunc.values.string(source[[scol]],20),")")
  }))
  names(choices) = c("", nchoices)

  rhs = webforms::selectizeInputVector(paste0("sourceVar_",cols),choices = choices, extra.class = "sourceVarSelect")


  lhs = unlist(lapply(cols, function(col) {
    paste0(col,"<br>"," (", trunc.values.string(dest[[col]],20),")" )
  }))

  html = paste0("<table><tr><th>Dest</th><th>Source</th></tr>",
    paste0("<tr><td>", lhs, "</td><td>",rhs,"</td></tr>", collapse="\n"),
    "</table>"
  )

  classEventHandler("sourceVarSelect", function(id, value,..., app=getApp()) {
    args = list(...)
    code = sep.lines(getInputValue("codeEdit"))
    restore.point("sourceVarSelect")

    col = str.right.of(id,"sourceVar_")
    new.code = merge.lines(modify.code.line(code, source, dest, svar = value, dvar=col))
    updateAceEditor(app$session,"codeEdit",new.code)
    writeClipboard(new.code)

  })

  HTML(html)
}

modify.code.line = function(code, source, dest,svar, dvar) {
  restore.point("modify.code.line")
  if (svar==".empty") return(code)

  lines = str.starts.with(str.trim(code), dvar)


  if (length(lines)==0) return(NULL)
  if (length(lines)>1) {
    rhs = str.right.of(code[lines],dvar)
    lines = lines[str.starts.with(str.trim(rhs),"=")]
  }

  dchar = is.character(dest[[dvar]])
  schar = is.character(source[[svar]])

  if (dchar | schar) {
    dvals = sort(unique(dest[[dvar]]))
    svals = sort(unique(as.character(source[[svar]])))
    if (any(is.na(svals))) svals = c(na.omit(svals),NA)
    svals = rep(svals, length=length(dvals))
    dquote = if (dchar) '"'
    squote = if (schar) '"'
    str = paste0("\t",dvar, " = recode(", svar,",",
      paste0(dquote,dvals,dquote,' = ',squote,svals,squote, collapse=", "),"),")
  } else {
    str = paste0("\t",dvar, " = ", svar,",")
  }
  code[lines] = str
  code
}


make.start.mapping.code = function(source, dest) {
  cols = colnames(dest)
  is.char = sapply(dest, is.character)
  code = paste0("\t",cols, " = ", cols)
  for (col in which(is.char)) {
    vals = unique(dest[[col]])
    code[col] = paste0("\t",cols[col], " = recode(", cols[col],",",
      paste0('"',vals,'" = "',vals,'"', collapse=", "),")")
  }
  res = paste0("mutate(\n", paste0(code, collapse=",\n"),"\n)")
  res
}

