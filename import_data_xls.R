import_data_xls=function(parameters_path) {

  params=list()
  
#         set(comp.importa_data_xls,'Value',1)
import_profile=read.delim(parameters_path,sep=';',header=T,stringsAsFactors=F)
import_profile=as.data.frame(sapply(import_profile, function(x) gsub("\\\\", "/", x)))
params$dir= import_profile[1,2]
metadata_path= as.character(import_profile[3,2])
Experiments=read.delim(metadata_path,sep=';',header=T,stringsAsFactors=F)[,1]
Experiments=as.vector(Experiments[Experiments!=''])
# results=file.path(dirname(as.character(metadata_path[1])), 'Import_information.xls')
params$expno=import_profile[4,2]
params$processingno=import_profile[5,2]
signals_names=read.delim(as.character(import_profile[6,2]),header=F,stringsAsFactors=F)[,1]
signals_names=as.list(signals_names[signals_names!=''])
signals_codes=1:length(signals_names)


profile_folder_path= as.character(import_profile[7,2])
# j=1
# for i=1:length(profiles_dir)
# if ((strcmp(profiles_dir(i).name,'.')~=1) && (strcmp(profiles_dir(i).name,'..')~=1))
#   ROI=regexp(profiles_dir(i).name,'.xls','split')
# ROIs{j,:}=ROI{1}
# j=j+1
# end
# end
export_path=as.character(import_profile[8,2])
if(!dir.exists(export_path)) dir.create(export_path)
plots_path=file.path(export_path,'Plots2Check')
if(!dir.exists(plots_path)) dir.create(plots_path)


E_max= as.numeric(as.character(import_profile[9,2]))
P_max= as.numeric(as.character(import_profile[10,2]))
freq=as.numeric(as.character(import_profile[14,2]))
params$buck_step=as.numeric(as.character(import_profile[21,2]))
normalization= import_profile[11,2]
# comp.jres_mode= datax(12)
# comp.hnmr_library_path= text{19,2}
# input_repository= import_profile[20,2]

params$norm_AREA='N'
params$norm_PEAK='N'
params$norm_PEAK_left_ppm=12
params$norm_PEAK_right_ppm=-1
if (normalization==1) {
  params$norm_AREA='Y'
  params$norm_PEAK_left_ppm=11.53
  params$norm_PEAK_right_ppm=10.47
# type_normalization='AREA'
# limits_normalization=c(11.53,10.47)
} else if (normalization==2) {
  params$norm_AREA='Y'
  params$norm_PEAK_left_ppm=0.1
  params$norm_PEAK_right_ppm=-0.1
# type_normalization='AREA'
# limits_normalization=c(0.1,-0.1)
} else if (normalization==3) {
  params$norm_PEAK='Y'
  params$norm_PEAK_left_ppm=3.15
  params$norm_PEAK_right_ppm=2.95
#   type_normalization='PEAK'
# limits_normalization=c(3.15,2.95)
} else if (normalization==4) {
  params$norm_AREA='Y'
  # type_normalization='AREA'
# limits_normalization=c(12,-1)
} else if (normalization==5) {
}

alignment = import_profile[12,2]
params$glucose_alignment='N'
params$tsp_alignment='N'
params$peak_alignment='N'
params$ref_peak_pos=8.452;
if (alignment==1) {
  params$glucose_alignment='Y'
# type_alignment='Glucose'
} else if (alignment==2) {
  params$tsp_alignment='Y'
# type_alignment='TSP'
} else if (alignment==3) {
  params$peak_alignment='Y'
# type_alignment='Peak'
# alignment_pos=8.452
  
}

suppression=as.character(import_profile[13,2])
if (suppression=='') {
params$disol_suppression='N'
} else {
params$disol_suppression='Y'
# try
params$disol_suppression_ppm =as.numeric(strsplit(suppression,';|,')[[1]])
dim(params$disol_suppression_ppm)=c(length(params$disol_suppression_ppm)/2,2)
params$disol_suppression_ppm=t(params$disol_suppression_ppm)
}
# catch
# disp('Invalid suppression parameters')
# return
# end
# end

# params.buck_step=import_profile[21,2]
# comp.freq=import_profile[14,2]
# Meta=read.xlsx(metadata_path,1)
# comp.groups=Meta.data(:,1)
# comp.group_number=unique(Meta.data(:,1))
# comp.Metadata=0

# if size(Meta.data,2)>1
# comp.metadata_values=Meta.data(:,2:end)
# comp.metadata_labels=Meta.textdata(1,3:size(Meta.textdata,2))
# comp.Metadata=1
# end

# tryCatch {
#   
# } 
# [datax2,text2]=xlsread(text{2,2})
# data.data=datax2(2:end,:)
# data.axisscale{2}=datax2(1,:)
# data.label{1}=Meta.textdata(2:end,1)
# if (upper(params.glucose_alignment) == 'Y')
#   for i=1:size(data.data,1)
# [~,glycinevalue(i)]=max(data.data(i,ppm2pos(5.27,data.axisscale{2}):ppm2pos(5.16,data.axisscale{2})))
# end    #[aaaa,bbbb]=max(max(J.Data(:,4900:4950)))
# for i=1:size(data.data,1)
# data.data(i,50:end-50)=data.data(i,50+round(glycinevalue(i)-mean(glycinevalue)):end-50+round(glycinevalue(i)-mean(glycinevalue)))
# end
# elseif (upper(params.tsp_alignment) == 'Y')
# for i=1:size(data.data,1)
# [~,glycinevalue(i)]=max(data.data(i,ppm2pos(0.1,data.axisscale{2}):ppm2pos(-0.1,data.axisscale{2})))
# end    #[aaaa,bbbb]=max(max(J.Data(:,4900:4950)))
# for i=1:size(data.data,1)
# data.data(i,50:end-50)=data.data(i,50+round(glycinevalue(i)-mean(glycinevalue)):end-50+round(glycinevalue(i)-mean(glycinevalue)))
# end
# elseif (upper(params.peak_alignment) == 'Y')
# for i=1:size(data.data,1)
# [~,glycinevalue(i)]=max(data.data(i,ppm2pos(8.5,data.axisscale{2}):ppm2pos(8.4,data.axisscale{2})))
# end    #[aaaa,bbbb]=max(max(J.Data(:,4900:4950)))
# for i=1:size(data.data,1)
# data.data(i,50:end-50)=data.data(i,50+round(glycinevalue(i)-mean(glycinevalue)):end-50+round(glycinevalue(i)-mean(glycinevalue)))
# end
# end
# 
# if params.normalization_AREA=='Y'
# for i=1:size(data.data,1)
# normalization_AREA(i) = sum(data.data(i,ppm2pos(params.normalization_AREA_left_ppm,data.axisscale{2}):ppm2pos(params.normalization_AREA_right_ppm,data.axisscale{2})))
# end
# for i=1:size(data.data,1)
# data.data(i,:) = data.data(i,:)*mean(normalization_AREA)/normalization_AREA(i)
# end
# elseif params.normalization_PEAK=='Y'
# for i=1:size(data.data,1)
# normalization_PEAK(i) = max(data.data(i,ppm2pos(params.normalization_PEAK_left_ppm,data.axisscale{2}):ppm2pos(params.normalization_PEAK_right_ppm,data.axisscale{2})))
# end
# for i=1:size(data.data,1)
# data.data(i,:) = data.data(i,:)*mean(normalization_PEAK)/normalization_PEAK(i)
# end
# end
# if params.disol_suppression=='Y'
# for i=1:size(params.disol_suppression_ppm,1)
# data.data(:,ppm2pos(params.disol_suppression_ppm(i,1),data.axisscale{2}):ppm2pos(params.disol_suppression_ppm(i,2),data.axisscale{2}))=0
# end
# end
# 
# catch ME
# 
# try
# catch
# disp('Invalid input file')
# end
# end

# 
# 
# if comp.jres_mode~=1
# params.expno_jres= datax(13)
# comp.jres_library_path= text{18,2}
# if comp.jres_mode==3
# comp.jres_path= char(strcat(text{17,2},'\',num2str(params.expno_jres)))
          # J=ReadJresRef(params,comp.jres_path)
          # comp.ppmYJ=J.YAxis
          # else
          # comp.jres_path= ''
          # J=ReadJres3(data,1,params,comp.jres_mode,comp.jres_path)
          # comp.ppmYJ=J.YAxis
          # end
          # else
          # comp.jres_library_path= ''
          # end
          # 
          # comp.mean_spectra=mean(data.data) #Generm un espectre extra que sera la mitja de tots els espectres del experiment
          # comp.median_spectra=median(data.data) #Generem un espectre extra que sera la mediana de tots els espectres del experiment
          # 
imported_data=Metadata2Buckets(Experiments,params)
# autorun_data$dataset=imported_data$X
imported_data$buck_step=params$buck_step
imported_data$params$profile_folder_path=profile_folder_path
imported_data$params$metadata_path=metadata_path
imported_data$params$parameters_path=parameters_path

imported_data$signals_names=signals_names
imported_data$signals_codes=signals_codes
imported_data$Experiments=setdiff(Experiments,imported_data$not_loaded_experiments)
imported_data$E_max=E_max
imported_data$P_max=P_max
imported_data$params$plots_path=plots_path
imported_data$params$export_path=export_path

imported_data$freq=freq

# imported_data$autorun_data=autorun_data
         return(imported_data)

}
          
                      