#nalysis StepI: Extract Significant data form LEfSe analysis

Data=/Users/caiyuanyuan/Desktop/HFQ黄丰青/202107胆汁


### PLS-DA analysis
[ ! -d $Data/3.1_PLS_DA ] && mkdir $Data/3.1_PLS_DA;
for i in $(ls $Data/0.0_raw_data/*.data.txt)
do
	ID=$(basename $i);
	Rscript $Data/bin/A3.1_Plot_PLS_DA.r $i $Data/bin/${ID%.*.*}-grouping.info  $Data/3.1_PLS_DA 2;
done

### OPLS-DA by ropls
[ ! -d $Data/3.2_OPLS_DA ] && mkdir $Data/3.2_OPLS_DA;
cd $Data/3.2_OPLS_DA;
for i in $(ls $Data/0.0_raw_data/*.data.txt)
do
        ID=$(basename $i);
        Rscript $Data/bin/A3.2_Plot_OPLS_DA.ropls.r $i $Data/bin/${ID%.*.*}-grouping.info  $Data/3.2_OPLS_DA 2;
        mv Rplots.pdf $Data/3.2_OPLS_DA/${ID%.*.*}.OPLSDA.summary.pdf;
        mv Rplots1.pdf $Data/3.2_OPLS_DA/${ID%.*.*}.OPLSDA.xscore.pdf;
done
### OPLS-DA by muma
cd $Data/3.2_OPLS_DA;
for i in $(ls $Data/0.0_raw_data/*.data.txt)
do
        ID=$(basename $i);
        Rscript $Data/bin/A3.2_Plot_OPLS_DA.muma.r $i $Data/bin/${ID%.*.*}-grouping.info  $Data/3.2_OPLS_DA 2;
        mv $Data/3.2_OPLS_DA/OPLS-DAP/SPlot_OPLS-DA_P.pdf $Data/3.2_OPLS_DA/${ID%.*.*}.OPLSDA.splot.pdf;
        rm Rplot*.pdf;
        rm -r Groups/ OPLS-DAP/ PCA_Data_P/ Preprocessing_Data_P/;
done
