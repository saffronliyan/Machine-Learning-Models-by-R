################################################################################
#                   Bayesian Linear Regression Analysis
#                              12102014
#
#Package: BayesFactor
################################################################################
################################################################################
#                   Load the Bayesian packages
################################################################################
library(RGtk2, quietly = T)
library(cairoDevice,quietly = TRUE)
library(rattle,quietly = T)

library(BayesFactor, quietly = T)

################################################################################
#                          BN GUI
################################################################################
blrGUI <- function()
{
###The GUI title and size
blrWin<- gtkWindowNew(show = FALSE)
blrWin$resize(800,600)
blrWin$setTitle("Bayesian Model Comparison")

###gtkContainers
blrVBox1 <- gtkVBoxNew()
blrVBox2 <- gtkVBoxNew()
blrVBox3 <- gtkVBoxNew()

blrHBox <- gtkHBoxNew()
blrHBox1 <- gtkHBoxNew()
blrHBox2 <- gtkHBoxNew()

blrFrame1 <- gtkFrameNew("Operations")
blrFrame2 <- gtkFrameNew("Textview")
blrFrame3 <- gtkFrameNew("Plot")

###toolbar
toolbar <- gtkToolbarNew()        # a new tool bar

toolbar$setIconSize(3)  #adjust icon size in tool bar
toolbar$setStyle("icons")

execute = gtkToolButtonNewFromStock(GTK_STOCK_EXECUTE)
toolbar$insert(execute,-1)
executeToolTipText<- rattle:::Rtxt("Execute current selections.")
gtkWidgetSetTooltipText(execute, executeToolTipText)
gSignalConnect(execute,"clicked", on_execute_button_clicked)

clear <-gtkToolButtonNewFromStock(GTK_STOCK_CLEAR)
toolbar$insert(clear,-1)
clearToolTipText<- rattle:::Rtxt("Clear the output area.")
gtkWidgetSetTooltipText(clear, clearToolTipText)
gSignalConnect(clear,"clicked", on_clear_button_clicked)

save = gtkToolButtonNewFromStock(GTK_STOCK_SAVE)
toolbar$insert(save,-1)
saveToolTipText<- rattle:::Rtxt("Save current plot.")
gtkWidgetSetTooltipText(save, saveToolTipText)
gSignalConnect(save,"clicked", on_save_button_clicked)

export <-gtkToolButtonNewFromStock(GTK_STOCK_CONNECT)
toolbar$insert(export,-1)
exportToolTipText<- rattle:::Rtxt("Export the current textview and log file.")
gtkWidgetSetTooltipText(export, exportToolTipText)
gSignalConnect(export,"clicked", on_export_button_clicked)

###Notebook
blr_notebook <<- gtkNotebookNew()
###Numeric tab
reg_vbox1<- gtkVBoxNew()

reg_table1 <- gtkTableNew(rows = 3, columns = 3, homogeneous = T)

alternativeLabel <- gtkLabelNew("Set Alternative Model:")
alternativeLabelToolTipText<- rattle:::Rtxt("Set the Alternative Model for Bayes Factor Analysis.")
gtkWidgetSetTooltipText(alternativeLabel, alternativeLabelToolTipText)

alternativeComboBox <<- gtkComboBoxNewText()
alternativeComboBoxToolTipText<- rattle:::Rtxt("List of the Alternative Models.")
gtkWidgetSetTooltipText(alternativeComboBox, alternativeComboBoxToolTipText)
alternativeComboBox$popdown()
alternativeComboBox$appendText("Best Model (all)")
alternativeComboBox$appendText("Most Complex Model (top)")
alternativeComboBox$appendText("Intercept Only (bottom)")
alternativeComboBox$setActive(0)

show_best_model_button <- gtkButtonNewWithLabel("Best Models")
gSignalConnect(show_best_model_button , "clicked", on_show_best_model_button_clicked)
showBestModelToolTipText<- rattle:::Rtxt("Show the best 6 models.")
gtkWidgetSetTooltipText(show_best_model_button, showBestModelToolTipText)

show_worst_model_button <- gtkButtonNewWithLabel("Worst Models")
gSignalConnect(show_worst_model_button , "clicked", on_show_worst_model_button_clicked)
showWorstModelToolTipText<- rattle:::Rtxt("Show the worst 6 models.")
gtkWidgetSetTooltipText(show_worst_model_button, showWorstModelToolTipText)

show_all_model_button <- gtkButtonNewWithLabel("All Models")
gSignalConnect(show_all_model_button , "clicked", on_show_all_model_button_clicked)
showAllModelToolTipText<- rattle:::Rtxt("Show the all models.")
gtkWidgetSetTooltipText(show_all_model_button, showAllModelToolTipText)

#show_model_spin_button <<- gtkSpinButtonNew()
#show_model_spin_button$setSensitive(T)
#show_model_spin_button$setRange(min = 1, max = 10)
#showSpinToolTipText<- rattle:::Rtxt("Choose the number of models to be shown.")
#gtkWidgetSetTooltipText(show_model_spin_button, showSpinToolTipText)
show_specific_model_button <- gtkButtonNewWithLabel("Specified Model")
gSignalConnect(show_specific_model_button, "clicked", on_show_specific_model_button_clicked)
showSpecificModelToolTipText<- rattle:::Rtxt("Show Bayes Factor Analysis of specific models.")
gtkWidgetSetTooltipText(show_specific_model_button, showSpecificModelToolTipText)
show_specific_entry <<- gtkEntryNew()
showSpecificEntryToolTipText<- rattle:::Rtxt('Strings to specify the model in the format as "v1+v2+...".')
gtkWidgetSetTooltipText(show_specific_entry, showSpecificEntryToolTipText)

plot_button <- gtkButtonNewWithLabel("Comparison Plot")
gSignalConnect(plot_button, "clicked", on_plot_button_clicked)
plotToolTipText<- rattle:::Rtxt("Plot the Bayes Factor Analysis of multiple models")
gtkWidgetSetTooltipText(plot_button, plotToolTipText)

#plot_save_button <- gtkButtonNewWithLabel("Save Plot")
#gSignalConnect(plot_save_button ,"clicked", on_plot_save_button_clicked)
#plotSave1ToolTipText<- rattle:::Rtxt("Save the comparison plot.")
#gtkWidgetSetTooltipText(plot_save_button, plotSave1ToolTipText)
#
#reg_table1$attach(alternativeLabel,0,1,0,1,2,2,2,2)
#reg_table1$attach(alternativeComboBox,1,2,0,1,2,2,2,2)
reg_table1$attach(show_best_model_button,0,1,0,1,2,2,2,2)
reg_table1$attach(show_worst_model_button,1,2,0,1,2,2,2,2)
reg_table1$attach(show_all_model_button,2,3,0,1,2,2,2,2)
reg_table1$attach(show_specific_model_button,0,1,1,2,2,2,2,2)
reg_table1$attach(show_specific_entry,1,2,1,2,2,2,2,2)
reg_table1$attach(plot_button,0,1,2,3,2,2,2,2)
reg_table1$attach(alternativeComboBox,1,2,2,3,2,2,2,2)
#reg_table1$attach(plot_save_button,2,3,2,3,2,2,2,2)

reg_vbox1$packStart(reg_table1,T,T,5)

blr_notebook_label1<-gtkLabelNew()
blr_notebook_label1$setText("Linear Regression(numeric)")
blr_notebook$appendPage(reg_vbox1, tab.label = blr_notebook_label1)
###Categoric tab
#reg_vbox2<- gtkVBoxNew()
#
#blr_notebook_label2<-gtkLabelNew()
#blr_notebook_label2$setText("Linear Regression(categoric)")
#blr_notebook$appendPage(reg_vbox2, tab.label = blr_notebook_label2)

###Mixed tab
reg_vbox3<- gtkVBoxNew()

reg_table2 <- gtkTableNew(rows = 3, columns = 3, homogeneous = T)

show_specific_model_button2 <- gtkButtonNewWithLabel("Specified Model")
gSignalConnect(show_specific_model_button2, "clicked", on_show_specific_model_button2_clicked)
showSpecificModelToolTipText2<- rattle:::Rtxt("Show Bayes Factor Analysis of specific models.")
gtkWidgetSetTooltipText(show_specific_model_button2, showSpecificModelToolTipText2)
show_specific_entry2 <<- gtkEntryNew()
showSpecificEntryToolTipText2<- rattle:::Rtxt('Strings to specify the model in the format as y~v1+v2+....')
gtkWidgetSetTooltipText(show_specific_entry2, showSpecificEntryToolTipText2)


show_model_button <- gtkButtonNewWithLabel("Show Selected Model")
gSignalConnect(show_model_button , "clicked", on_show_model_button_clicked)
showModelToolTipText<- rattle:::Rtxt("Show the selected model.")
gtkWidgetSetTooltipText(show_model_button, showModelToolTipText)

modelComboBox <<- gtkComboBoxNewText(T)
modelComboBox$popdown()
modelComboBoxToolTipText<- rattle:::Rtxt("Please choose a model.")
gtkWidgetSetTooltipText(modelComboBox, modelComboBoxToolTipText)
modelComboBox$appendText("Model1: Full Model")
modelComboBox$setActive(0)
modelIndex <<- 0
txt2 <<-list(NULL,NULL,NULL,NULL)
allBFs <<-list(NULL,NULL,NULL,NULL)

plot_button2 <- gtkButtonNewWithLabel("Comparison Plot")
gSignalConnect(plot_button2, "clicked", on_plot_button2_clicked)
plotToolTipText2<- rattle:::Rtxt("Plot the Bayes Factor Analysis against the best model.")
gtkWidgetSetTooltipText(plot_button2, plotToolTipText2)

reg_table2$attach(show_specific_model_button2,0,1,0,1,2,2,2,2)
reg_table2$attach(show_specific_entry2,1,2,0,1,2,2,2,2)
reg_table2$attach(show_model_button,0,1,1,2,2,2,2,2)
reg_table2$attach(modelComboBox,1,2,1,2,2,2,2,2)
reg_table2$attach(plot_button2,0,1,2,3,2,2,2,2)

reg_vbox3$packStart(reg_table2,T,T,5)

blr_notebook_label3<-gtkLabelNew()
blr_notebook_label3$setText("General Linear Regression(mixed) ")
blr_notebook$appendPage(reg_vbox3, tab.label = blr_notebook_label3)

blrFrame1$add(blr_notebook)
###Texview
blr_textview<<-gtkTextViewNew(show = TRUE)
blr_textview$modifyFont(pangoFontDescriptionFromString(crv$textview.font))
blr_scrolledWindow<- gtkScrolledWindowNew()
blr_scrolledWindow$add(blr_textview)

blrHBox2$packStart(blr_scrolledWindow,T,T,5)

blrFrame2$add(blrHBox2)
###plot window
blr_drawingArea<<-gtkDrawingAreaNew()
blr_drawingArea$setSizeRequest(600,400)
blr_drawingArea$modifyBg("normal","white")

blrVBox3$packStart(blr_drawingArea, T, T, 0)

blrFrame3$add(blrVBox3)
###Add up

blrVBox2$packStart(blr_notebook,T,T,5)

blrVBox1$packStart(toolbar, F,T,5)
blrVBox1$packStart(blrHBox, T,T,5)
blrHBox$packStart(blrVBox2, T,T,5)
blrHBox$packStart(blrFrame3, T,T,5)
blrVBox2$packStart(blrFrame1, F,T,5)
blrVBox2$packStart(blrFrame2, T,T,5)

blrWin$add(blrVBox1)
blrWin$show()

#data(attitude)
#crs$bf <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
#show_model_spin_button$setSensitive(T)
#show_model_spin_button$setRange(min = 1, max = length(crs$bf) )


}

on_execute_button_clicked<- function(button)
{
cur.page <- blr_notebook$getCurrentPage()
 if(cur.page == 0){
data(attitude)
crs$dataset <- attitude
crs$dataname <- "attitude"
crs$target <- "rating"
bf.cmd <- sprintf('crs$bf <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "all", progress = FALSE)', crs$target)
#alt <- alternativeComboBox$getActive()

#if(alt == 0)
#   {bf.cmd <- sprintf('crs$bf <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "all")', crs$target)
#}else if (alt == 1)
#   {bf.cmd <- sprintf('crs$bf <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "top")', crs$target)
#}else if (alt == 2)
#   {bf.cmd <- sprintf('crs$bf <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "bottom")', crs$target)
#}
 eval(parse(text=bf.cmd))
 } else if (cur.page == 1)
 {
data(ToothGrowth)
crs$dataset <- ToothGrowth
crs$dataname <- "ToothGrowth"
crs$target <- "len"
bf.cmd <- sprintf('crs$bf <- lmBF(formula=%s~.,data =crs$dataset,progress = FALSE)', crs$target)
eval(parse(text=bf.cmd))
allBFs <<- crs$bf
 }

preText<<-rattle:::Rtxt("The Bayes Factor K is interpreted as follows:\nK < 3, the alternative model is not supported.\nK >= 3 and K < 10,the alternative model is supported.\nK > 10, the alternative model is strongly supported.\nK > 100, the alternative model is decisive.\n\n\n")
}
on_show_best_model_button_clicked <- function(button)
{
#show_model_spin_button$setSensitive(T)
#model_num <- show_model_spin_button$getValueAsInt()

#modelShow.cmd <- head(crs$bf, as.numeric(model_num))
modelShow.cmd <- sprintf("head(crs$bf,6)")

buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show the best six models:"), "\n\n"), rattle:::collectOutput(modelShow.cmd, TRUE),
              rattle:::textviewSeparator())
}

on_show_worst_model_button_clicked <- function(button)
{
#show_model_spin_button$setSensitive(T)
#model_num <- show_model_spin_button$getValueAsInt()

#modelShow.cmd <- head(crs$bf, as.numeric(model_num))
modelShow.cmd <- sprintf("tail(crs$bf,6)")

buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show the worst six models:"), "\n\n"), rattle:::collectOutput(modelShow.cmd, TRUE),
              rattle:::textviewSeparator())
}

on_show_all_model_button_clicked <- function(button)
{
#show_model_spin_button$setSensitive(T)
#model_num <- show_model_spin_button$getValueAsInt()

#modelShow.cmd <- head(crs$bf, as.numeric(model_num))
modelShow.cmd <- sprintf("crs$bf")

buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show all models:"), "\n\n"), rattle:::collectOutput(modelShow.cmd, TRUE),
              rattle:::textviewSeparator())
}

on_show_specific_model_button_clicked <- function(button)
{
txt <- show_specific_entry$getText()
modelSpec.cmd <- sprintf("crs$bf[%s]", txt)
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show Bayes Factor Analysis of the Specified Model:"), "\n\n"), rattle:::collectOutput(modelSpec.cmd, TRUE),
              rattle:::textviewSeparator())
}
on_plot_button_clicked <- function(button)
{
#asCairoDevice(blr_drawingArea)
alt <- alternativeComboBox$getActive()
if(alt ==0) {
#plot(head(crs$bf)/max(crs$bf))
bf2.cmd <- sprintf("bf2<-head(crs$bf)/max(crs$bf)")
eval(parse(text=bf2.cmd))
asCairoDevice(blr_drawingArea)
frame()
plot(bf2)
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, paste(rattle:::Rtxt("Show Bayes factor Analysis compared with the best model:"), "\n"), rattle:::collectOutput(bf2.cmd, TRUE),
              rattle:::textviewSeparator())
 } else if (alt == 1){
bf2.cmd <- sprintf('bf2 <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "top", progress = FALSE)', crs$target)
eval(parse(text=bf2.cmd))
asCairoDevice(blr_drawingArea)
frame()
plot(bf2)
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, paste(rattle:::Rtxt("Show Bayes factor Analysis compared with the most complex model:"), "\n"), rattle:::collectOutput(bf2.cmd, TRUE),
              rattle:::textviewSeparator())
              } else if (alt == 2) {
bf2.cmd <- sprintf('bf2 <- regressionBF(formula=%s~.,data =crs$dataset,whichModels = "bottom", progress = FALSE)', crs$target)
eval(parse(text=bf2.cmd))
asCairoDevice(blr_drawingArea)
frame()
plot(bf2)
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, paste(rattle:::Rtxt("Show Bayes factor Analysis compared with the intercept only Model:"), "\n"), rattle:::collectOutput(bf2.cmd, TRUE),
              rattle:::textviewSeparator())
}
#eval(parse(text=bf2.cmd))
#asCairoDevice(blr_drawingArea)
##frame()
##plot(bf2)
#x <- seq(-4, 4, length=100)
#hx <- dnorm(x)
#
#plot(x, hx, type="l", lty=2, xlab="x value",
#  ylab="Density", main="Comparison of t Distributions")

#plot(bf2)



}

on_save_button_clicked <- function(action)
{
  ttl<-"BayesianPlot"
  rattle:::savePlotToFileGui(rattle:::dev.num(ttl))

}

on_export_button_clicked <- function(button)
{

dialog <- gtkFileChooserDialog(title = "Export Bayesian Network", parent = NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  dialog$setDoOverwriteConfirmation(TRUE)

  ff <- gtkFileFilterNew()
  ff$setName(rattle:::Rtxt("Text Files"))
  ff$addPattern("*.txt")
  dialog$addFilter(ff)

  #ff <- gtkFileFilterNew()
#  ff$setName(rattle:::Rtxt("PMML Files"))
#  ff$addPattern("*.xml")
#  dialog$addFilter(ff)

#   ext <- tolower(rattle:::get.extension(save.name))

  dialog$setCurrentName(sprintf("%s_bayesfactor.txt", crs$dataname))
  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
#    save.name <- rattle:::getExportSaveName(crv$RPART)
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

#  rattle:::startLog(paste(Rtxt("Export"), rattle:::commonName(crv$RPART)))
#   crv$BNL <- "Bayesian Network"
 #  save.name <- rattle:::getExportSaveName(crv$BNL)
   if (is.null(save.name)) return(FALSE)
   ext <- tolower(rattle:::get.extension(save.name))

#  if (cur.page == 1)
#  {

if (ext == "txt"){

  log.buf <- blr_textview$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  save.text <-log.buf$getText(start, end)
  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
  write(save.text, save.name)
  } else if (ext == "xml"){
  }
#  }else if (cur.page == 2)
#  {
#
#if (ext == "txt"){
#
#  log.buf <- param_textview1$getBuffer()
#  start <- log.buf$getStartIter()$iter
#  end <- log.buf$getEndIter()$iter
#  save.text <-log.buf$getText(start, end)
#  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
#  write(save.text, save.name)  }
#  }
#  else if (ext == "xml")
#  {
#
#  }
#  }
#
}

on_clear_button_clicked<-function(button)
{
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)
frame()
}

on_show_specific_model_button2_clicked <- function(button)
{
txt2[[modelIndex+1]] <<- show_specific_entry2$getText()
modelSpec.cmd <- sprintf("crs$bf <- lmBF(formula = %s,data = crs$dataset, progress = FALSE)", txt2[[modelIndex+1]])
buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show Bayes factor Analysis of the Specified Model:"), "\n"), rattle:::collectOutput(modelSpec.cmd, TRUE),
              rattle:::textviewSeparator())

modelComboBox$appendText(sprintf("Model%d: %s",modelIndex+2,txt2[[modelIndex+1]]))
modelIndex <<- modelIndex+1
eval(parse(text=modelSpec.cmd))
allBFs <<- c(allBFs, crs$bf)
}

on_show_model_button_clicked <- function(button)
{
 activeIndex <- modelComboBox$getActive()
 if(activeIndex == 0){modelShow.cmd <- sprintf('crs$bf <- lmBF(formula=%s~.,data =crs$dataset,progress = FALSE)', crs$target)
 }else{
 modelShow.cmd <- sprintf("crs$bf <- lmBF(formula = %s,data = crs$dataset, progress = FALSE)", txt2[[activeIndex]]) }
 buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, preText,paste(rattle:::Rtxt("Show Bayes factor Analysis of the Specified Model:"), "\n"), rattle:::collectOutput(modelShow.cmd, TRUE),
              rattle:::textviewSeparator())

}
on_plot_button2_clicked <- function(button)
{
# if(modelIndex==0){maxModel.cmd <-sprintf('crs$bf <- lmBF(formula=%s~.,data =crs$dataset,progress = FALSE)', crs$target)  }
# else { }
maxBF <- max(allBFs)
modelShow.cmd <- "allBFs/maxBF"

asCairoDevice(blr_drawingArea)
plot(allBFs/maxBF)

buffer <- gtkTextBufferNew()
blr_textview$setBuffer(buffer)

rattle:::addTextview(blr_textview, paste(rattle:::Rtxt("Show Bayes factor Analysis compared with best Model:"),"\n"),rattle:::collectOutput(modelShow.cmd, TRUE),
              rattle:::textviewSeparator())


}
#data(attitude)
#crs$bf <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
#show_model_spin_button$setRange(min = 1, max = length(crs$bf) )
blrGUI()
#source("C:\\work\\Bayesian\\Bayesian Linear Regression Analysis GUI.R")
