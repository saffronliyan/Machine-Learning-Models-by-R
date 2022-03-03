################################################################################
#                            Bayesian Network GUI
#                           by Yan Li @ Oct 28 2014
#
#Packages: bnlearn; gRain; Rgraphviz; graph
#Functionalities: Different learning algorithm;Bayesian inference (Naïve Bayes)
#Plot: DAG; other plots for bn.fit including(histogram, qq, xyplot...)
#
#
################################################################################
################################################################################
#                   Load the Bayesian packages
################################################################################
library(RGtk2, quietly = T)
library(cairoDevice,quietly = TRUE)
library(rstat,quietly = TRUE)
library(bnlearn,quietly = TRUE)
#library(gRain,quietly = TRUE)
library(Rgraphviz, quietly = TRUE)
library(graph, quietly = TRUE)
library(pmml,quietly = TRUE)
################################################################################
#                          BN GUI
################################################################################
bnlGUI <- function()
{
###The GUI title and size
bnlWin<- gtkWindowNew(show = FALSE)
bnlWin$resize(800,600)
bnlWin$setTitle("Bayesian Network GUI")

###gtkContainers
bnl_hbox1 <- gtkHBoxNew()
bnl_vbox0 <- gtkVBoxNew()
bnl_vbox1 <- gtkVBoxNew()
bnl_vbox2 <- gtkVBoxNew()
bnl_hbox2 <- gtkHBoxNew()
bnl_hbox3 <- gtkHBoxNew()
bnl_hbox4 <- gtkHBoxNew()
bnl_hbox5 <- gtkHBoxNew()
bnl_hbox6 <- gtkHBoxNew()

###gtkFrames
bnl_frame1 <- gtkFrameNew("Data")
bnl_frame2 <- gtkFrameNew("Study")
bnl_frame3 <- gtkFrameNew("Operations")
bnl_frame4 <- gtkFrameNew("Plot")
bnl_frame5 <- gtkFrameNew("Textview&Log")

###Data frame
#bnl_Table1 <- gtkTableNew(rows = 3, columns = 3, homogeneous = T)
data_hbox <- gtkHBoxNew()
dataLabel <- gtkLabelNew("Load Data:")
#fileButton <<- gtkFileChooserButton("Select Your File","clicked")
dataComboBox <<- gtkComboBoxNewText()
dataComboBox$appendText("learning.test")
dataComboBox$appendText("alarm")
dataComboBox$appendText("gaussian.test")
dataComboBox$setActive(0)

#dataButton<- gtkButtonNewWithLabel("Summary and Plot")
#gSignalConnect(dataButton,"clicked",on_data_button_clicked)
#bnl_Table1$attach(dataLabel,0,1,1,2)
#bnl_Table1$attach(fileButton,1,2,1,2)
data_hbox$packStart(dataLabel,T,T,5)
data_hbox$packStart(dataComboBox,T,T,5)

data_vbox <- gtkVBoxNew()
data_vbox$packStart(data_hbox,F,F,5)


#bnl_hbox2$packStart(bnl_Table1,T,T,0)
#bnl_frame1$add(bnl_hbox2)
#bnl_frame1$add(data_hbox)

bnl_notebook<<-gtkNotebookNew()

bnl_notebook_label0<-gtkLabelNew()
bnl_notebook_label0$setText("Data")
bnl_notebook$appendPage(data_vbox, tab.label = bnl_notebook_label0)


###Algorithms frame


###learning tab
learning_table <- gtkTableNew(rows = 3, columns = 5 , homogeneous = F)
learning_hbox1 <-gtkHBoxNew()
learning_hbox2 <-gtkHBoxNew()
learning_hbox3 <-gtkHBoxNew()
learning_vbox1 <- gtkVBoxNew()

learningLabel <- gtkLabelNew("Learning Algorithms:")
learningToolTipText<- rattle:::Rtxt("Structure Learning Algorithms")
gtkWidgetSetTooltipText(learningLabel, learningToolTipText)
learningComboBox <<- gtkComboBoxNewText(T)
learningComboBox$popdown()
learningToolTipText2<- rattle:::Rtxt("Please choose an algorithm.")
gtkWidgetSetTooltipText(learningComboBox, learningToolTipText2)

learningComboBox$appendText("Grow-Shrink (gs)")
learningComboBox$appendText("Incremental Association (iamb)")
learningComboBox$appendText("Fast Incremental Association (fast.iamb)")
learningComboBox$appendText("Interleaved Incremental Association (inter.iamb)")
learningComboBox$appendText("Hill-Climbing (hc)")
learningComboBox$appendText("Tabu Search (tabu)")
learningComboBox$appendText("Max-Min Hill-Climbing (mmhc)")
learningComboBox$appendText("Restricted Maximization (rsmax2)")
learningComboBox$appendText("Max-Min Parents and Children (mmpc)")
learningComboBox$appendText("Hiton Parents and Children (si.hiton.pc)")
learningComboBox$appendText("Chow-Liu (chow.liu)")
learningComboBox$appendText("ARACNE (aracne)")
learningComboBox$setActive(0)

arcOperationLabel <- gtkLabelNew("Set Arc:")
arcOperationToolTipText<- rattle:::Rtxt("Set arc direction or reverse it.")
gtkWidgetSetTooltipText(arcOperationLabel, arcOperationToolTipText)

fromLabel <- gtkLabelNew("From")
fromEntry <<- gtkEntryNew()
fromToolTipText<- rattle:::Rtxt("the node label representing the start of arc.")
gtkWidgetSetTooltipText(fromEntry, fromToolTipText)

toLabel <- gtkLabelNew("to")
toEntry <<- gtkEntryNew()
toToolTipText<- rattle:::Rtxt("the node label representing the end of arc.")
gtkWidgetSetTooltipText(toEntry, toToolTipText)

setArcButton <- gtkButtonNewWithLabel("Set")
setArcToolTipText<- rattle:::Rtxt("set the arc directionbased on the from node and to node.")
gtkWidgetSetTooltipText(setArcButton, setArcToolTipText)
gSignalConnect(setArcButton , "clicked", on_setArcButton_clicked)

reverseArcButton <- gtkButtonNewWithLabel("Reverse")
reverseArcToolTipText<- rattle:::Rtxt("reverse the arc direction based on the from node and to node.")
gtkWidgetSetTooltipText(reverseArcButton, reverseArcToolTipText)
gSignalConnect(reverseArcButton , "clicked", on_reverseArcButton_clicked)

#learning_table$attach(learningLabel,0,1,0,1,0,0,0,0)
#learning_table$attach(learningComboBox,1,5,0,1,0,0,0,0)
#learning_table$attach(arcOperationLabel,0,1,1,2,0,0,0,0) 
#learning_table$attach(fromLabel,1,2,1,2,0,0,0,0)
#learning_table$attach(fromEntry,2,3,1,2,0,0,0,0)
#learning_table$attach(toLabel,3,4,1,2,0,0,5,0)
#learning_table$attach(toEntry,4,5,1,2,0,0,5,0)
#learning_table$attach(setArcButton,0,1,2,3,0,0,0,0)
#learning_table$attach(reverseArcButton,1,2,2,3,0,0,0,0)

bnl_textview1<<-gtkTextViewNew(show = TRUE)
bnl_textview1$modifyFont(pangoFontDescriptionFromString(crv$textview.font))
bnl_scrolledWindow1<- gtkScrolledWindowNew()
bnl_scrolledWindow1$add(bnl_textview1)

bnl_separator1 <- gtkHSeparatorNew()

#bnl_table2$attach(learningLabel,0,1,0,1,0,0,0,0)
#bnl_table2$attach(learningComboBox,1,2,0,1,0,0,0,0)
#bnl_table2$attach(bnl_separator1,0,3,1,2,0,0,0,0)
#bnl_table2$attach(bnl_scrolledWindow1,0,3,3,10,0,0,0,0)

learning_hbox1$packStart(learningLabel,T,T,2.5)
learning_hbox1$packStart(learningComboBox,T,T,2.5)
learning_hbox2$packStart(arcOperationLabel,T,T,2.5)
learning_hbox2$packStart(fromLabel,T,F,2.5)
learning_hbox2$packStart(fromEntry,T,F,2.5)
learning_hbox2$packStart(toLabel,T,F,2.5)
learning_hbox2$packStart(toEntry,T,F,2.5)

learning_hbox3$packStart(setArcButton,F,T,2.5)
learning_hbox3$packStart(reverseArcButton,F,T,2.5)

learning_vbox1$packStart(learning_hbox1,F,T,2.5)
learning_vbox1$packStart(learning_hbox2,F,T,2.5)
learning_vbox1$packStart(learning_hbox3,F,T,2.5)
#learning_vbox1$packStart(learning_table,F,T,2.5)
learning_vbox1$packStart(bnl_separator1,F,T,2.5)
learning_vbox1$packStart(bnl_scrolledWindow1,T,T,2.5)

bnl_notebook_label1<-gtkLabelNew()
bnl_notebook_label1$setText("Structure Learning")
#bnl_notebook$appendPage(bnl_table2, tab.label = bnl_notebook_label1)
bnl_notebook$appendPage(learning_vbox1, tab.label = bnl_notebook_label1)

###Parameter Estimation Tab
#param_vbox <- gtkVBoxNew()


param_hbox1 <-gtkHBoxNew()
param_vbox1 <- gtkVBoxNew()

paramLabel <- gtkLabelNew("Method:")
paramToolTipText<- rattle:::Rtxt("Parameter Estimation Method.")
gtkWidgetSetTooltipText(paramLabel, paramToolTipText)
paramComboBox <<- gtkComboBoxNewText(T)
paramComboBox$popdown()
paramToolTipText1<- rattle:::Rtxt("Please choose a parameter estimation method.")
gtkWidgetSetTooltipText(paramComboBox, paramToolTipText1)
paramComboBox$appendText("Maximum Likelihood parameter estimation (mle)")
paramComboBox$appendText("Bayesian parameter estimation (bayesian;dicrete data)")
paramComboBox$setActive(0)

param_textview1<<-gtkTextViewNew(show = TRUE)
param_textview1$modifyFont(pangoFontDescriptionFromString(crv$textview.font))
param_scrolledWindow1<- gtkScrolledWindowNew()
param_scrolledWindow1$add(param_textview1)

param_separator1 <- gtkHSeparatorNew()

param_hbox1$packStart(paramLabel,T,T,2.5)
param_hbox1$packStart(paramComboBox,T,T,2.5)
param_vbox1$packStart(param_hbox1,F,T,2.5)
param_vbox1$packStart(param_separator1,F,T,2.5)
param_vbox1$packStart(param_scrolledWindow1,T,T,2.5)


bnl_notebook_label3<-gtkLabelNew()
bnl_notebook_label3$setText("Parameter Estimation")
bnl_notebook$appendPage(param_vbox1, tab.label = bnl_notebook_label3)

###Inference Tab
bnl_table3 <- gtkTableNew(rows = 4, columns = 4 , homogeneous = F)

inference_hbox1 <-gtkHBoxNew()
inference_hbox2 <-gtkHBoxNew()
inference_vbox1 <- gtkVBoxNew()

typeLabel <- gtkLabelNew("Querry Types:")
typeToolTipText<- rattle:::Rtxt("Querry probability or random observations.")
gtkWidgetSetTooltipText(typeLabel, typeToolTipText)
querryRadioButton1 <<- gtkRadioButtonNew()
querryRadioButton1$setLabel("Probability")
gSignalConnect(querryRadioButton1, "toggled", on_querryRadioButton1_toggled)
querryRadioButton2 <<- gtkRadioButtonNewWithLabelFromWidget(querryRadioButton1,"Observations")
gSignalConnect(querryRadioButton2, "toggled", on_querryRadioButton2_toggled)

querryPlotButton <<- gtkButtonNewWithLabel("Plot")
plotToolTipText<- rattle:::Rtxt("Histogram plot.")
gtkWidgetSetTooltipText(querryPlotButton, plotToolTipText)
querryPlotButton$setSensitive(F)
gSignalConnect(querryPlotButton, "clicked", on_querryPlotButton_clicked)

inferenceLabel <- gtkLabelNew("Event:")
inferenceToolTipText<- rattle:::Rtxt("Strings to describ the event of interest.")
gtkWidgetSetTooltipText(inferenceLabel, inferenceToolTipText)
inferenceEntryBox1 <<- gtkEntryNew()
#inferenceComboBox <<- gtkComboBoxNewText(T)
#inferenceComboBox$popdown()
#
#inferenceComboBox$appendText("Naive Bayes (naive.bayes)")
#inferenceComboBox$appendText("Tree-Augmented Naive Bayes (tree.bayes)")
#inferenceComboBox$setActive(0)

inferenceLabel2 <- gtkLabelNew("Evidence:")
inferenceToolTipText2<- rattle:::Rtxt("Strings of the conditioning evidence.")
gtkWidgetSetTooltipText(inferenceLabel2, inferenceToolTipText2)
inferenceEntryBox2 <<- gtkEntryNew()
#inferenceComboBox2 <<- gtkComboBoxNewText(T)
#inferenceComboBox2$popdown()

bnl_textview2<<-gtkTextViewNew(show = TRUE)
bnl_textview2$modifyFont(pangoFontDescriptionFromString(crv$textview.font))
bnl_scrolledWindow2<- gtkScrolledWindowNew()
bnl_scrolledWindow2$add(bnl_textview2)

bnl_separator2 <- gtkHSeparatorNew()

bnl_table3$attach(typeLabel,0,1,0,1,2,2,2,2)
bnl_table3$attach(querryRadioButton1,1,2,0,1,2,2,2,2)
bnl_table3$attach(querryRadioButton2,2,3,0,1,2,2,2,2)
bnl_table3$attach(querryPlotButton,3,4,0,1,2,2,2,2)
bnl_table3$attach(inferenceLabel,0,1,1,2,2,2,2,2)
bnl_table3$attach(inferenceEntryBox1,1,4,1,2,2,2,2,2)
bnl_table3$attach(inferenceLabel2,0,1,2,3,2,2,2,2)
bnl_table3$attach(inferenceEntryBox2,1,4,2,3,2,2,2,2)

#inference_hbox1$packStart(inferenceLabel,T,T,2.5)
#inference_hbox1$packStart(inferenceEntryBox1,T,T,2.5)
#inference_hbox2$packStart(inferenceLabel2,T,T,2.5)
#inference_hbox2$packStart(inferenceEntryBox2,T,T,2.5)
#inference_vbox1$packStart(inference_hbox1,F,T,2.5)
#inference_vbox1$packStart(inference_hbox2,F,T,2.5)
inference_vbox1$packStart(bnl_table3,F,T,2.5)
inference_vbox1$packStart(bnl_separator2,F,T,2.5)
inference_vbox1$packStart(bnl_scrolledWindow2,T,T,2.5)

bnl_notebook_label2<-gtkLabelNew()
bnl_notebook_label2$setText("Inference")
bnl_notebook$appendPage(inference_vbox1, tab.label = bnl_notebook_label2)



bnl_hbox3$packStart(bnl_notebook,T,T,0)
bnl_frame2$add(bnl_hbox3)



###Operation frame
bnl_frame3$add(bnl_hbox4)

###Plot frame

bnl_drawingArea<<-gtkDrawingAreaNew()
bnl_drawingArea$setSizeRequest(400,400)
bnl_drawingArea$modifyBg("normal","white")

bnl_hbox5$packStart(bnl_drawingArea, T, T, 0)
bnl_frame4$add(bnl_hbox5)

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

###Textview frame
bnl_frame5$add(bnl_hbox6)

###Add up
#bnl_vbox1$packStart(bnl_frame1,F,T,0)
bnl_vbox1$packStart(bnl_frame2,T,T,0)
#bnl_vbox1$packStart(bnl_frame5,T,T,5)

bnl_vbox2$packStart(bnl_frame4,T,T,5)
#bnl_vbox2$packStart(bnl_frame5,T,T,5)

bnl_hbox1$packStart(bnl_vbox1,T,T,5)
bnl_hbox1$packStart(bnl_vbox2,T,T,5)
                                   #
bnl_vbox0$packStart(toolbar,F,F,0)
bnl_vbox0$packStart(bnl_hbox1,T,T,5)

bnlWin$add(bnl_vbox0)

bnlWin$show()
}

on_execute_button_clicked <- function(button)
{
TV <- bnl_textview1
mtype <- "Bayesian Network"

#if (rattle:::noDatasetLoaded()) return(FALSE)

#filename <- fileButton$getUri()
#if(is.null(filename)) {rattle:::noDatasetLoaded();return(FALSE)}

cur.page <- bnl_notebook$getCurrentPage()
if(cur.page == 0)
{
data<- dataComboBox$getActive()

if(data == 0){data(learning.test);crs$dataset <- learning.test;crs$dataname <- "learning_test"}
if(data == 1) {data(alarm);crs$dataset <-alarm;crs$dataname<-"alarm"}
if(data == 2){data(gaussian.test);crs$dataset <- gaussian.test;crs$dataname<-"gaussian_test"}

#inferenceComboBox2$getModel()$clear()
#
#train.var <- sort(names(crs$dataset))
#for ( i in 1: length(train.var))
#		inferenceComboBox2$appendText(train.var[i])
#
##inferenceComboBox2$appendText("A")
##inferenceComboBox2$appendText("B")
##inferenceComboBox2$appendText("C")
#inferenceComboBox2$setActive(0)
}
else if(cur.page == 1)###structure learning
{
learning.alg <- learningComboBox$getActive()
if (learning.alg == 0)
       {learning.cmd <- sprintf("gs(crs$dataset)")
          crs$bnl <-gs(crs$dataset) }
else if (learning.alg == 1)
       {learning.cmd <- sprintf("iamb(crs$dataset)")
          crs$bnl <-iamb(crs$dataset) }
else if (learning.alg == 2)
        {learning.cmd <- sprintf("fast.iamb(crs$dataset)")
	    crs$bnl <-fast.iamb(crs$dataset)}
else if(learning.alg == 3)
        {learning.cmd <- sprintf("inter.iamb(crs$dataset)")
	   crs$bnl <-inter.iamb(crs$dataset)}
else if (learning.alg == 4)
         {learning.cmd <- sprintf("hc(crs$dataset)")
	   crs$bnl <-hc(crs$dataset)}
else if (learning.alg == 5)
         {learning.cmd <- sprintf("tabu(crs$dataset)")
	   crs$bnl <-tabu(crs$dataset)}
else if (learning.alg == 6)
         {learning.cmd <- sprintf("mmhc(crs$dataset)")
	   crs$bnl <-mmhc(crs$dataset)}
else if (learning.alg == 7)
           {learning.cmd <- sprintf("rsmax2(crs$dataset)")
	    crs$bnl <-rsmax2(crs$dataset)}
else if (learning.alg == 8)
           {learning.cmd <- sprintf("mmpc(crs$dataset)")
	    crs$bnl <-mmpc(crs$dataset)}
else if (learning.alg == 9)
           {learning.cmd <- sprintf("si.hiton.pc(crs$dataset)")
	     crs$bnl <-si.hiton.pc(crs$dataset)}
else if (learning.alg == 10)
           {learning.cmd <- sprintf("chow.liu(crs$dataset)")
	    crs$bnl <-chow.liu(crs$dataset)}
else if (learning.alg == 11)
           {learning.cmd <- sprintf("aracne(crs$dataset)")
	    crs$bnl <-aracne(crs$dataset)}

buffer <- gtkTextBufferNew()
bnl_textview1$setBuffer(buffer)

rattle:::addTextview(bnl_textview1, paste(rattle:::Rtxt("Bayesian Network Learning:"), "\n"), collectOutput(learning.cmd, TRUE),
              rattle:::textviewSeparator())

#rattle:::addTextview(chi_textview2, paste(rattle:::Rtxt("Goodness of fit test:"), "\n"), chi.cmd,
#              rattle:::textviewSeparator())

asCairoDevice(bnl_drawingArea)
node.num<-length(crs$bnl$nodes)
if(node.num <= 10)
  plot(crs$bnl)
else
graphviz.plot(crs$bnl)
}
else if(cur.page == 2)###parameter estimation
{
param.alg <- paramComboBox$getActive()
if (param.alg == 0)
       {param.cmd <- sprintf("bn.fit(crs$bnl,crs$dataset,method = 'mle')")
          crs$bn.fit <-bn.fit(crs$bnl,crs$dataset,method = "mle") }
else if (param.alg == 1)
       {param.cmd <- sprintf("bn.fit(crs$bnl,crs$dataset,method = 'bayesian')")
          crs$bn.fit <-bn.fit(crs$bnl,crs$dataset,method = "bayesian") }

buffer <- gtkTextBufferNew()
param_textview1$setBuffer(buffer)

rattle:::addTextview(param_textview1, paste(rattle:::Rtxt("Parameter Estimation:"), "\n"), collectOutput(param.cmd, TRUE),
              rattle:::textviewSeparator())

#rattle:::addTextview(chi_textview2, paste(rattle:::Rtxt("Goodness of fit test:"), "\n"), chi.cmd,
#              rattle:::textviewSeparator())

asCairoDevice(bnl_drawingArea)
node.num<-length(crs$bnl$nodes)
if(node.num <= 10)
  plot(crs$bnl)
else
graphviz.plot(crs$bnl)

#title(main = sprintf("%s_Bayesian NetWork",crs$dataname))
}
else if (cur.page == 3)
{

#training <- inferenceComboBox2$getActiveText()
#inference.alg <- inferenceComboBox$getActive()
#if(inference.alg == 0)
#      {inference.cmd <- sprintf("naive.bayes(crs$dataset,'%s')",training)
#        crs$nb <- naive.bayes(crs$dataset,training) }
#if(inference.alg == 1)
#      {inference.cmd <- sprintf("tree.bayes(crs$dataset,'%s')",training)
#crs$nb <- tree.bayes(crs$dataset,training)}

event <- inferenceEntryBox1$getText()
evidence <- inferenceEntryBox2$getText()
querryType<- ifelse(querryRadioButton1$getActive(), 0,1)
if(querryType == 0){
inference.cmd <- sprintf("cpquery(crs$bn.fit,(%s),(%s))",event,evidence)  } else {
inference.cmd <- sprintf("cpdist(crs$bn.fit,(%s),(%s))",event,evidence)  }

buffer <- gtkTextBufferNew()
bnl_textview2$setBuffer(buffer)

rattle:::addTextview(bnl_textview2, paste(rattle:::Rtxt("The estimated probability or observations are:"), "\n"), collectOutput(inference.cmd, TRUE),
              rattle:::textviewSeparator())

#rattle:::addTextview(chi_textview2, paste(rattle:::Rtxt("Goodness of fit test:"), "\n"), chi.cmd,
#              rattle:::textviewSeparator())

#asCairoDevice(bnl_drawingArea)
#node.num<-length(crs$nb$nodes)
#if(node.num <= 10)
#  plot(crs$nb)
#else
#  graphviz.plot(crs$nb)

}

}


on_clear_button_clicked<-function(button)
{
buffer <- gtkTextBufferNew()
bnl_textview1$setBuffer(buffer)
bnl_textview2$setBuffer(buffer)
frame()
}

on_save_button_clicked <- function(button)
{
  ttl<-"Plot"
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

   cur.page <- bnl_notebook$getCurrentPage()
   if(cur.page ==1){
   dialog$setCurrentName(sprintf("%s_bnl.txt", crs$dataname))
   }else if (cur.page ==2){
   dialog$setCurrentName(sprintf("%s_cpt.txt", crs$dataname))
   }else if (cur.page ==3){dialog$setCurrentName(sprintf("%s_inference.txt", crs$dataname))
   }else {}

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
   crv$BNL <- "Bayesian Network"
 #  save.name <- rattle:::getExportSaveName(crv$BNL)
   if (is.null(save.name)) return(FALSE)
   ext <- tolower(rattle:::get.extension(save.name))

  if (cur.page == 1)
  {
#   if(rattle:::not.null(crs$dataname))
#    dialog$setCurrentName(sprintf("%s_BNL.txt", rattle:::get.stem(crs$dataname)))

  #ff <- gtkFileFilterNew()
#  ff$setName(rattle:::Rtxt("Text Files"))
#  ff$addPattern("*.txt")
#  dialog$addFilter(ff)

#  learning.alg <- learningComboBox$getActive()
#if (learning.alg == 0)
#       {learning.cmd <- sprintf("gs(crs$dataset)")
#          crs$bnl <-gs(crs$dataset) }
#else if (learning.alg == 1)
#       {learning.cmd <- sprintf("iamb(crs$dataset)")
#          crs$bnl <-iamb(crs$dataset) }
#else if (learning.alg == 2)
#        {learning.cmd <- sprintf("fast.iamb(crs$dataset)")
#	    crs$bnl <-fast.iamb(crs$dataset)}
#else if(learning.alg == 3)
#        {learning.cmd <- sprintf("inter.iamb(crs$dataset)")
#	   crs$bnl <-inter.iamb(crs$dataset)}
#else if (learning.alg == 4)
#         {learning.cmd <- sprintf("hc(crs$dataset)")
#	   crs$bnl <-hc(crs$dataset)}
#else if (learning.alg == 5)
#         {learning.cmd <- sprintf("tabu(crs$dataset)")
#	   crs$bnl <-tabu(crs$dataset)}
#else if (learning.alg == 6)
#         {learning.cmd <- sprintf("mmhc(crs$dataset)")
#	   crs$bnl <-mmhc(crs$dataset)}
#else if (learning.alg == 7)
#           {learning.cmd <- sprintf("rsmax2(crs$dataset)")
#	    crs$bnl <-rsmax2(crs$dataset)}
#else if (learning.alg == 8)
#           {learning.cmd <- sprintf("mmpc(crs$dataset)")
#	    crs$bnl <-mmpc(crs$dataset)}
#else if (learning.alg == 9)
#           {learning.cmd <- sprintf("si.hiton.pc(crs$dataset)")
#	     crs$bnl <-si.hiton.pc(crs$dataset)}
#else if (learning.alg == 10)
#           {learning.cmd <- sprintf("chow.liu(crs$dataset)")
#	    crs$bnl <-chow.liu(crs$dataset)}
#else if (learning.alg == 11)
#           {learning.cmd <- sprintf("aracne(crs$dataset)")
#	    crs$bnl <-aracne(crs$dataset)}
if (ext == "txt"){
  #save.text <- collectOutput(learning.cmd, TRUE)
#  txt.cmd <- sprintf("save.text <- collectOutput(%s,TRUE)\n",learning.cmd)
#  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
#  rattle:::appendLog(sprintf(Rtxt("Export %s as TXT."), rattle:::commonName(crv$BNL)),
#              txt.cmd,
#              'gsub("\\n\\n+", "\\n", gsub("#[^\\n]*\\n", "", save.text))\n',
#              sprintf('write(save.text, "%s")', save.name))
#  write(save.text, save.name)
#  dialog$setCurrentName(sprintf("%s_bnl.txt", crs$dataname))
#  if (dialog$run() == GtkResponseType["accept"])
#  {
#    save.name <- dialog$getFilename()
##    save.name <- rattle:::getExportSaveName(crv$RPART)
#    dialog$destroy()
#  }
#  else
#  {
#    dialog$destroy()
#    return()
#  }
#   if (is.null(save.name)) return(FALSE)
#   ext <- tolower(rattle:::get.extension(save.name))
#
  log.buf <- bnl_textview1$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  save.text <-log.buf$getText(start, end)
  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
  write(save.text, save.name)
  } else if (ext == "xml"){
  }
  }else if (cur.page == 2)
  {
#param.alg <- paramComboBox$getActive()
#if (param.alg == 0)
#       {param.cmd <- sprintf("bn.fit(crs$bnl,crs$dataset,method = 'mle')")
#          crs$bn.fit <-bn.fit(crs$bnl,crs$dataset,method = "mle") }
#else if (param.alg == 1)
#       {param.cmd <- sprintf("bn.fit(crs$bnl,crs$dataset,method = 'bayesian')")
#          crs$bn.fit <-bn.fit(crs$bnl,crs$dataset,method = "bayesian") }
if (ext == "txt"){
  #save.text <- collectOutput(param.cmd, TRUE)
#  txt.cmd <- sprintf("save.text <- collectOutput(%s,TRUE)\n",param.cmd)
#  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
#  rattle:::appendLog(sprintf(Rtxt("Export %s as TXT."), rattle:::commonName(crv$BNL)),
#              txt.cmd,
#              'gsub("\\n\\n+", "\\n", gsub("#[^\\n]*\\n", "", save.text))\n',
#              sprintf('write(save.text, "%s")', save.name))
#  dialog$setCurrentName(sprintf("%s_cpt.txt", crs$dataname))
#  if (dialog$run() == GtkResponseType["accept"])
#  {
#    save.name <- dialog$getFilename()
##    save.name <- rattle:::getExportSaveName(crv$RPART)
#    dialog$destroy()
#  }
#  else
#  {
#    dialog$destroy()
#    return()
#  }
#   if (is.null(save.name)) return(FALSE)
#   ext <- tolower(rattle:::get.extension(save.name))

  log.buf <- param_textview1$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  save.text <-log.buf$getText(start, end)
  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
  write(save.text, save.name)  }
  }
  else if (cur.page == 3)
  {
#  dialog$setCurrentName(sprintf("%s_naive bayes.txt", rattle:::get.stem(crs$dataname)))

  #ff <- gtkFileFilterNew()
#  ff$setName(rattle:::Rtxt("Text Files"))
#  ff$addPattern("*.txt")
#  dialog$addFilter(ff)
#
#  ff <- gtkFileFilterNew()
#  ff$setName(rattle:::Rtxt("PMML Files"))
#  ff$addPattern("*.xml")
#  dialog$addFilter(ff)

  #training <- inferenceComboBox2$getActiveText()
#  inference.alg <- inferenceComboBox$getActive()
#  if(inference.alg == 0)
#      {inference.cmd <- sprintf("naive.bayes(crs$dataset,'%s')",training)
#        crs$nb <- naive.bayes(crs$dataset,training) }
#  if(inference.alg == 1)
#      {inference.cmd <- sprintf("tree.bayes(crs$dataset,'%s')",training)
#   crs$nb <- tree.bayes(crs$dataset,training)}
# event <- inferenceEntryBox1$getText()
#evidence <- inferenceEntryBox2$getText()
#inference.cmd <- sprintf("cpquery(crs$bn.fit,(%s),(%s))",event,evidence)
#
   if (ext == "txt"){
#  save.text <- collectOutput(inference.cmd, TRUE)
#  txt.cmd <- sprintf("save.text <- collectOutput(%s,TRUE)\n",inference.cmd)
#  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
#  rattle:::appendLog(sprintf(Rtxt("Export %s as TXT."), rattle:::commonName(crv$BNL)),
#              txt.cmd,
#              'gsub("\\n\\n+", "\\n", gsub("#[^\\n]*\\n", "", save.text))\n',
#              sprintf('write(save.text, "%s")', save.name))
#  write(save.text, save.name)
#dialog$setCurrentName(sprintf("%s_inference.txt", crs$dataname))
#  if (dialog$run() == GtkResponseType["accept"])
#  {
#    save.name <- dialog$getFilename()
##    save.name <- rattle:::getExportSaveName(crv$RPART)
#    dialog$destroy()
#  }
#  else
#  {
#    dialog$destroy()
#    return()
#  }
#   if (is.null(save.name)) return(FALSE)
#   ext <- tolower(rattle:::get.extension(save.name))

log.buf <-bnl_textview2$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  save.text <-log.buf$getText(start, end)
  save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
  write(save.text, save.name)
   } else if (ext == "xml")
  {
  ###the pmml.naiveBayes is able to generate a pmml for nb from package e1071

    #pmml.cmd <- sprintf("pmml:::pmml.naiveBayes(crs$nb%s, dataset=crs$dataset)",
#                      ifelse(length(crs$transforms),
#                             ", transforms=crs$transforms", ""))
#
#    rattle:::appendLog(Rtxt("Export regression as PMML."),
#              sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
#    saveXML(eval(parse(text=pmml.cmd)), save.name)
#
  }
  }

}
on_querryRadioButton1_toggled <- function(button)
{querryPlotButton$setSensitive(F)
 #querryPlotButton$setActive(F)
}
on_querryRadioButton2_toggled <- function(button)
{querryPlotButton$setSensitive(T)
 #querryPlotButton$setActive(F)
}
on_querryPlotButton_clicked <- function(button)
{
asCairoDevice(bnl_drawingArea)
event <- inferenceEntryBox1$getText()
evidence <- inferenceEntryBox2$getText()
#if(is.numeric(crs$dataset[[event]])){
#hist(cpdist(crs$bn.fit, event,evident)[,1]) }else{
#hist(cpdist(crs$bn.fit, event,evidence))#}
inferencePlot.cmd <- sprintf("hist(cpdist(crs$bn.fit,(%s),(%s)));title(main ='%s histogram plot')",event,evidence,crs$dataname)
eval(parse(text=inferencePlot.cmd))
}

on_setArcButton_clicked <- function(button)
{
from <- fromEntry$getText()
to <- toEntry$getText()

set.cmd <- sprintf("set.arc(crs$bnl,%s,%s)",from,to)
eval(parse(text=set.cmd))
}

on_reverseArcButton_clicked <- function(button)
{
from <- fromEntry$getText()
to <- toEntry$getText()

reverse.cmd <- sprintf("reverse.arc(crs$bnl,%s,%s)",from,to)
eval(parse(text=reverse.cmd))
}


bnlGUI()
#source("C:\\work\\Bayesian\\Bayesian Network UI_2.R")
