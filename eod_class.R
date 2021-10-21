require(reader)
require(ggplot2)
require(changepoint)
require("stringr") # regular expressions library


global_metadata_lines = 12
global_sampling_rate = 192000
global_bit_depth = 16
global_encoding="utf-8"
global_treshold <- 0.02
global_nb_peaks<-7
global_arbitrary_baseline <-40
global_q10<-1.5
global_q10_target_temperature<-25

global_baseline_done = FALSE
global_baseline_type="meanvar"
global_time_alignment_type="halfway"

Eod <-setRefClass("Eod",
                  fields = list(
                    file = "character",
                    threshold= "numeric",
                    maxPeaks= "numeric",
                    specimenTag = "character",
                    provisionalId="character",
                    collectionEvent= "character",
                    recordingSoftware = "character",
                    recordingHardware = "character",
                    recordist = "character",
                    bitDepth = "numeric",
                    samplingRate = "numeric",
                    recordingDateTime="ANY",
                    recordingTemperature="numeric",
                    recordingConductivity="numeric",
                    projectName = "character", 
                    wave = "ANY",
                    metadata_lines = "numeric",
                    obj_creation_time="ANY",
                    file_metadata = "ANY",
                    encoding = "character",
                    file_wave="ANY",
                    decimal_sep = "character",
                    specimenIdentifier="character",
                    #SC treatements
                    o_changepoints_mean="ANY",
                    v_changepoints_mean = "vector",
                    o_changepoints_var="ANY",
                    v_changepoints_var = "vector",
                    o_changepoints_meanvar="ANY",
                    v_changepoints_meanvar = "vector",
                    mean_by_mean="numeric",
                    mean_by_var="numeric",
                    mean_by_meanvar="numeric",
                    mean_by_arbitrary_baseline="numeric",
                    last_by_mean="numeric",
                    last_by_var="numeric",
                    last_by_meanvar="numeric",
                    last_by_arbitrary_baseline="numeric",
                    arbitraryBaseline="numeric",
                    frame_baseline="ANY",
                    frame_absolute="ANY",
                    baselineType = "character",
                    chosen_baseline_index="numeric",
                    chosen_baseline="ANY",
                    chosen_baseline_position="numeric",
                    chosen_last_position="numeric",
                    normalized_wave= "ANY",
                    normalized_wave_amplitude_only= "ANY", 
                    v_frame_zero = "ANY",
                    frame_start_end ="ANY",
                    valleys_peaks = "ANY",
                    padded_normalized_wave_for_periodgram = "ANY",
                    periodgram_frame = "ANY",
                    periodgram = "ANY",
                    main_plot="ANY",
                    periodgram_plot = "ANY",
                    derivate_plot = "ANY", 
                    base_filename="character",
					time_alignment="character",
					biggest_normalized_time_interval="numeric",
					q10="numeric",
					q10_target_temperature="numeric"
                  )
                  
)

atts <- attributes(Eod$fields())$names
Eod$accessors(atts)
Eod$methods()

Eod$methods(
  initialize=
    function(specimenTag="N/A",
             samplingRate =  global_sampling_rate ,
             metadata_lines = global_metadata_lines, 
             provisionalId="N/A",
             collectionEvent="N/A",
             recordingSoftware ="N/A",
             recordingHardware="N/A" ,
             recordist="N/A",
             bitDepth =global_bit_depth ,
             recordingDateTime = "N/A",
             recordingTemperature = 0,
             recordingConductivity = 0,
             projectName = "N/A", 
             encoding = global_encoding,
             threshold = global_treshold,
             maxPeaks = global_nb_peaks,
             arbitraryBaseline= global_arbitrary_baseline,
             baselineType=global_baseline_type,
			 time_alignment=global_time_alignment_type,
			 is_normalized=FALSE,
			 biggest_normalized_time_interval=0,
			 q10=global_q10,
			 q10_target_temperature=global_q10_target_temperature,
             ...
    )
    {
      callSuper(...)
      specimenTag <<- specimenTag
      provisionalId <<- provisionalId
      collectionEvent <<- collectionEvent
      recordingSoftware <<- recordingSoftware
      recordingHardware <<- recordingHardware
      recordist <<- recordist
      bitDepth <<-  bitDepth
      samplingRate <<-  samplingRate
      recordingDateTime <<-  recordingDateTime
      recordingTemperature <<-  recordingTemperature
      recordingConductivity <<-  recordingConductivity
      threshold <<- threshold
      maxPeaks <<-maxPeaks
      #tech
      metadata_lines <<- metadata_lines
      projectName <<- projectName
      encoding <<-encoding
      obj_creation_time <<- Sys.time()
      #sc
      arbitraryBaseline<<-arbitraryBaseline
      v_changepoints_mean <<-c(0)
      v_changepoints_var <<-c(0)
      v_changepoints_meanvar <<-c(0)
      mean_by_mean<<-0
      mean_by_var<<-0
      mean_by_meanvar<<-0
      mean_by_arbitrary_baseline<<-0
      last_by_mean<<-0
      last_by_var<<-0
      last_by_meanvar<<-0
      last_by_arbitrary_baseline<<-0
      baselineType <<- baselineType 
      base_filename<<-basename(file)
      specimenIdentifier<<- paste(projectName, collectionEvent, specimenTag, sep="_")
	  time_alignment<<- time_alignment
	  biggest_normalized_time_interval<<-0
	  q10<<-q10
	  q10_target_temperature<<-q10_target_temperature
      .self
    }
  ,
  readFile = function(file, encoding=global_encoding)
  {
    file <<- file
    encoding <<-encoding
    file_obj_tmp <- read.csv(file, 
                             header=FALSE, 
                             nrows=metadata_lines, 
                             sep="=", 
                             colClasses=c("character","character"),
                             fileEncoding = encoding)
    
    #remove extra columns
    if(ncol(file_obj_tmp)>2)
    {
      file_obj_tmp<-file_obj_tmp[,1:2]
    }
    
    colnames(file_obj_tmp) <- c("param", "value")
    file_metadata<<-file_obj_tmp
    initialize(
      specimenTag=file_metadata$value[1],
      provisionalId=file_metadata$value[2],
      collectionEvent=file_metadata$value[3],
      recordingSoftware=file_metadata$value[4],
      recordingHardware=file_metadata$value[5],
      recordist=file_metadata$value[6],
      bitDepth=as.numeric(file_metadata$value[7]),
      samplingRate = as.numeric(file_metadata$value[8]),
      recordingDateTime = file_metadata$value[9],
      recordingTemperature = as.numeric(file_metadata$value[10]),
      recordingConductivity =  as.numeric(file_metadata$value[11]),
      projectName =   file_metadata$value[12],
    )
    #read wave
    decimal_sep <<- get.delim(file,
                              skip=metadata_lines,
                              delims=c(".",",")
    )
    file_wave<<-read.csv(
      file,
      sep='\t',
      skip=metadata_lines, 
      header=TRUE,
      dec=decimal_sep,
      colClasses=c("numeric","numeric", "NULL")
    )
    if(ncol(file_wave)>2)
    {
      file_wave<<-file_wave[,1:2]
    }
    colnames(file_wave) <<- c("time", "amplitude")
  },
  #geters and setters
  getMetadata=function()
  {
    file_metadata
  },
  setMetadata=function(metadata)
  {
    file_metadata<<-metadata
  },
  getQ10=function()
  {
    q10
  },
  setQ10=function(q10)
  {
    q10<<-q10
  },
  getQ10TargetTemperature=function()
  {
    q10_target_temperature
  },
  setQ10TargetTemperature=function(q10_target_temperature)
  {
    q10_target_temperature<<-q10_target_temperature
  },
  getBaseFilename=function()
  {
    base_filename
  },
  setFile=function(f_file)
  {
	file<<-f_file
	base_filename<<-f_file
  },
  getWave=function()
  {
    file_wave
  },
  setWave=function(p_wave)
  {
    file_wave<<-p_wave
  },
  getNormalizedWave=function()
  {
    normalized_wave
  },
  getBiggestNormalizedTimeInterval=function()
  {
	biggest_normalized_time_interval
  },
  getMainPlot = function()
  {
    main_plot
  }
  ,
  getDerivatePlot = function()
  {
    derivate_plot
  },
  getPeriodgramPlot = function()
  {
    periodgram
  },
  getT1T2= function()
  {
    frame_start_end
  },
  getLandmarks= function()
  {
    tmp<-normalized_wave[valleys_peaks,]
    tmp[order(tmp$time),]
  },
  getNormalizedBaseLine=function()
  {
	normalized_baseline_level
  },
  getRecordingDateFormatted = function()
  {
    timestamp(recordingDateTime)
  },
  #sc functions
  defineBaseline=function()
  {
    
    
    o_changepoints_mean<<-cpt.mean(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_mean<<-cpts(o_changepoints_mean)
    
    if(length(v_changepoints_mean)>0)
    {  
      mean_by_mean <<- coalesce(mean(file_wave[1:v_changepoints_mean[1],1 ]),0L )
    }
    else
    {
      v_changepoints_mean<<-c(1,length(file_wave$time))
    }
    last_by_mean <<- tail(v_changepoints_mean, n=1)
    
    o_changepoints_var<<-cpt.var(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_var<<-cpts(o_changepoints_var)
    if(length(v_changepoints_var)>0)
    { 
      mean_by_var <<- mean(file_wave[1:v_changepoints_var[1],1 ])
    }
    else
    {
      v_changepoints_var<<-c(1,length(file_wave$time))      
    }
    last_by_var <<- tail(v_changepoints_var, n=1)
    
    o_changepoints_meanvar<<-cpt.meanvar(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_meanvar <<- cpts(o_changepoints_meanvar)
    if(length(v_changepoints_meanvar)>0)
    { 
      mean_by_meanvar <<- mean(file_wave[1:v_changepoints_meanvar[1],1 ])
    }
    else
    {
      v_changepoints_meanvar<<-c(1,length(file_wave$time))
    }
    last_by_meanvar <<- tail(v_changepoints_meanvar, n=1)
    mean_by_arbitrary_baseline <<- mean(file_wave[1:arbitraryBaseline, 1 ])
    last_by_arbitrary_baseline <<-nrow(file_wave)
    
    global_baseline_done = TRUE
    
  },
  getAbsoluteValues=function()
  {
    
    frame_absolute <<- data.frame(
      entry=c("duration_sec", "nb_lines", "min_amplitude", "max_amplitude", "pos_min", "pos_max", "time_min", "time_max"),
      value=c(tail(file_wave$time,1), 
              format(length(file_wave$time), scientific=FALSE), 
              min(file_wave$amplitude), max(file_wave$amplitude),
              format(which(file_wave$amplitude==min(file_wave$amplitude)), scientific=FALSE),
              format(which(file_wave$amplitude==max(file_wave$amplitude)), scientific=FALSE),
              file_wave$time[which(file_wave$amplitude==min(file_wave$amplitude))],
              file_wave$time[which(file_wave$amplitude==max(file_wave$amplitude))]
      )
      
    )
    frame_absolute
  }
  ,
  getPossibleBaseline=function(type="meanvar",q10_corr=FALSE, q10=global_q10, q10_target_temperature=global_q10_target_temperature)
  {
    
    if(global_baseline_done==FALSE)
    {
      defineBaseline()
      
    }
    
    frame_baseline <<- data.frame(
      type=c("mean", "var", "meanvar","by_arbitrary_baseline" ),
      mean=c(mean_by_mean,mean_by_var,mean_by_meanvar,mean_by_arbitrary_baseline ), 
      first_position=c(v_changepoints_mean[1],v_changepoints_var[1], v_changepoints_meanvar[1],arbitraryBaseline),
      last_position=c(last_by_mean, last_by_var, last_by_meanvar,last_by_arbitrary_baseline),
      first_position_time=c(file_wave$time[ v_changepoints_mean[1]],
                            file_wave$time[v_changepoints_var[1]], 
                            file_wave$time[v_changepoints_meanvar[1]],
                            file_wave$time[arbitraryBaseline]),
      last_position_time=c(file_wave$time[last_by_mean], file_wave$time[last_by_var], file_wave$time[last_by_meanvar],file_wave$time[last_by_arbitrary_baseline]),
      inflexion_points=c(paste(v_changepoints_mean,collapse=" "),paste(v_changepoints_var,collapse=" "), paste(v_changepoints_meanvar,collapse=" "),paste(c(arbitraryBaseline,last_by_arbitrary_baseline),collapse=" "))
    )
    
    chooseBaselineAndNormalize(type,q10_corr, q10, q10_target_temperature)
    frame_baseline
  },
  isNormalized=function()
  {
	is_normalized
  },
  setTimeAlignment=function(type)
  {
	time_alignment<<-type
  }
  ,
  getTimeAlignment=function()
  {
	time_alignment
  },
  #main sc function
  normalize = function(type=baselineType)
  {
    
    getPossibleBaseline(type)
  },
  biggest_time_interval=function(frame)
  {
    init<-FALSE
	ref_time<-NULL
	ref_interval<-0
	apply(frame,1, 
		function(x)
				{
					if(init)
					{
						new_time<-x[1]
						tmp_interval<-abs(new_time-ref_time)
						if(tmp_interval>ref_interval)
						{
							ref_interval<<-tmp_interval
						}
					}
					else
					{
						init<<-TRUE
					}
					ref_time<<-x[1]
					
				})
	ref_interval
  },
  detect_landmarks_after_normalization=function()
  {
	#createPeriodgram()
    
    #View(normalized_wave)
    signal_limit = start_end_signal()
    #print("SIGNAL LIMIT")
    #print(signal_limit)
    v_start_time_signal = normalized_wave$time[signal_limit[1]]
    #print(v_start_time_signal)
    v_end_time_signal = normalized_wave$time[signal_limit[2]]
    #print(v_end_time_signal)
    #print("signal duration=")
    #print(v_end_time_signal - v_start_time_signal)
    frame_start_end  <<- normalized_wave[signal_limit, ]
    frame_start_end$labels <<- c("T1","T2")
    
    v_zero_point<-which(abs(normalized_wave$time)==min(abs(normalized_wave$time)))
    v_frame_zero <<- normalized_wave[v_zero_point,]
    
    valleys_peaks <<- find_peaks_and_valleys(normalized_wave,chosen_baseline_position,chosen_last_position-1, p_nb_peaks=maxPeaks )
    #print(valleys_peaks)
    #main plot
    main_plot <<- save_plot(normalized_wave, paste("Wave and landmarks (normalized)",base_filename), valleys_peaks, v_frame_zero, frame_start_end, chosen_baseline_position )
    
    #deriv plot
    tmp_frame_deriv<-fct_diff_deriv(normalized_wave)
    v_frame_deriv_start_end  <- tmp_frame_deriv[signal_limit, ]
    v_frame_deriv_start_end$labels <- c("T1","T2")
    v_frame_deriv_zero <- tmp_frame_deriv[v_zero_point,]
    derivate_plot <<- save_plot(tmp_frame_deriv, paste("Derivate", base_filename), valleys_peaks, v_frame_deriv_zero, v_frame_deriv_start_end, chosen_baseline_position )
    print("EOD state : normalized")
	biggest_normalized_time_interval<<-biggest_time_interval(normalized_wave)
	print("biggest normalized time interval")
	print(biggest_normalized_time_interval)
	is_normalized<<-TRUE
  },
  applyQ10 =function(q10, q10_target_temperature)
  {
	if(!is.null(recordingTemperature))
	{
		if(recordingTemperature!=0)
		{
			print(q10_target_temperature)
			print(recordingTemperature)
			normalized_wave$time<<- normalized_wave$time * q10 ^((q10_target_temperature-recordingTemperature)/10)
		}
	}
  }

  ,
  chooseBaselineAndNormalize = function(type,q10_corr=FALSE, q10=global_q10, q10_target_temperature=25) #type = "mean, meanvar, var, arbitrary
  {
  
    #print(type)
    baselineType<<-type
    if(baselineType=="mean")
    {
      chosen_baseline_index <<- 1
    }
    else if(baselineType=="var")
    {
      chosen_baseline_index <<- 2
    }
    else if(baselineType=="meanvar")
    {
      chosen_baseline_index <<- 3
    }
    else if(baselineType=="arbitrary")
    {
      chosen_baseline_index <<-4
    }
    
    chosen_baseline<<-frame_baseline$mean[chosen_baseline_index]
	normalized_baseline_level<<-chosen_baseline
    chosen_baseline_position<<-frame_baseline$first_position[chosen_baseline_index]
    chosen_last_position<<-frame_baseline$last_position[chosen_baseline_index]
    
    #normalize amplitude
    normalized_wave <<- data.frame(file_wave)
	if(q10_corr)
	{
		applyQ10(q10, q10_target_temperature)
	}
    
    normalized_wave$amplitude <<- normalized_wave$amplitude - chosen_baseline
    
    normalized_wave$amplitude <<- normalized_wave$amplitude/(max(normalized_wave$amplitude) - min(normalized_wave$amplitude))
    normalized_wave_amplitude_only <<-normalized_wave
    #normalize time
	#print("TRY_TO_NORMALIZE_TIME")
	#print(time_normalization)
    #normalized_wave<<-normalize_and_center_time(normalized_wave, time_normalization)
	#no time normalization
	normalized_wave<<-center_time(normalized_wave, time_alignment)
    #View(normalized_wave)
	detect_landmarks_after_normalization()
    
  },
  
  start_end_signal = function()
  {
    
    tmp_amplitude <- abs(normalized_wave$amplitude)
    signal <- which(tmp_amplitude > threshold)
    c(min(signal), max(signal))
  },
  peak_detection = function(p_frame, start_index,last_position, p_threshold=NULL, diff_threshold=NULL)
  {
    v_position=c()
    v_prominence=c()
    previous_peak=0
    #v_prominence <-c()
    for(i in start_index:last_position )
    {
      p_val=p_frame$amplitude[i]
      p_previous=p_frame$amplitude[i-1]
      p_next=p_frame$amplitude[i+1]
      if(p_val > p_previous && p_val == p_next)
      {
        i<-detect_peak_plateau(p_frame, i, nrow(p_frame)-1)
        p_next=p_frame$amplitude[i+1]
      }
      if(p_val > p_previous && p_val > p_next )
      {
        if(!is.null(p_threshold))
        {
          if(p_val<=p_threshold)
          {
            next
          }
        }
        if(!is.null(diff_threshold))
        {
          if((p_val-p_previous )<=diff_threshold)
          {
            next
          }
        }
        
        v_prominence<-c(v_prominence, abs(p_val-previous_peak)) 
        v_position=c(v_position, i)
        previous_peak=p_val
      }
      
    }
    v_returned<-data.frame(position=v_position, prominence=v_prominence)
    v_returned
  },
  
  find_peaks_and_valleys = function( p_frame, p_silent_index, p_last_position, p_diff_threshold=NULL, p_nb_peaks=global_nb_peaks)
  {
    
    
    o_peaks<-peak_detection(p_frame, p_silent_index,p_last_position, diff_threshold =p_diff_threshold )
    p_frame$amplitude=(p_frame$amplitude) * - 1
    o_peaks2<-peak_detection(p_frame, p_silent_index,p_last_position, p_threshold=0,diff_threshold =p_diff_threshold)
    
    v_all_submits=rbind(o_peaks, o_peaks2)
    v_all_submits<-v_all_submits[order(v_all_submits$prominence),]
    
    v_peaks=v_all_submits$position
    
    v_peaks<-unique(v_peaks)
    
    v_peaks<-tail(v_peaks, n=p_nb_peaks)
    
    v_peaks
  }
  ,
  detect_peak_plateau = function(p_frame, current_position, last_position)
  {

    returned<-current_position
    p_ref_val=p_frame$amplitude[current_position]
 
	#bracket for (current_position+1) very important otherwise analysis goes beyond wave !
	#because : for(i in current_position+1:last_position)
	#is evaluated as : for(i in current_position+(1:last_position)) !
    for(i in (current_position+1):last_position)
    {
	 
      p_val<-p_frame$amplitude[i]
	  
      if(p_val!=p_ref_val)
      {
        return(i-1)
      }
    }
    returned
  },
  pad_normalized=function(duration, direction)
  {
	#closest
	duration<-min(normalized_wave$time)+duration
	closest<-which.min(abs(duration-normalized_wave$time))
	
	if(direction=="right")
	{
		shifter<-c( rep(normalized_baseline_level, closest), normalized_wave$amplitude[(seq(1,nrow(normalized_wave)-closest))])
	}
	else if(direction=="left")
	{
		shifter<-c( normalized_wave$amplitude[(seq(closest,nrow(normalized_wave)))],rep(normalized_baseline_level, closest) )
	}
	normalized_wave$amplitude <<- head(shifter,nrow(normalized_wave))
	
	detect_landmarks_after_normalization()
  },
  fct_diff_deriv=function(p_frame)
  {
    v_returned=c()
    v_diff=diff(p_frame$amplitude)*(samplingRate/1000)
    v_returned=c(v_returned,0 )
    for(i in 2:length(v_diff))
    {
      v_returned=c(v_returned,v_diff[i]-v_diff[i-1] )
    }
    v_returned=c(v_returned,0 )
    
    p_frame$amplitude<-v_returned
    
    p_frame  
  }
  ,
  #FREQUENCY FCT
  eod_spectrum =function()
  {
    
    
    spec<-spectrum(padded_normalized_wave_for_periodgram$amplitude,fs= samplingRate , plot=FALSE)
    
    periodgram_frame <<-data.frame(freq=spec$freq,spec=spec$spec )
    periodgram_frame$freq <<- periodgram_frame$freq * samplingRate
    periodgram <<-ggplot(periodgram_frame, aes(x=periodgram_frame$freq, y=10*log10(periodgram_frame$spec/max(periodgram_frame$spec)) )) + geom_line() + scale_x_log10() +ggtitle(paste("Periodgram", base_filename))
    
    
  }
  ,
  pad_1_sec=function()
  {
    v_diff_1sec=1-max(normalized_wave_amplitude_only$time)
    #print("diff 1 sec")
    #print(v_diff_1sec)
    
    v_time_interv=diff(normalized_wave_amplitude_only$time)
    v_mean_interval=mean(v_time_interv)
    #print("mean interv")
    #print(v_mean_interval)
    #View(v_time_interv)
    v_pad=seq(0,v_diff_1sec/2, by=v_mean_interval )
    #View(pad)
    #copy
    v_1sec_frame=data.frame(normalized_wave_amplitude_only)
    v_1sec_frame$time<-v_1sec_frame$time + v_mean_interval+ (v_diff_1sec/2) 
    v_pad_frame=data.frame(time=v_pad, amplitude=rep(0, times=length(v_pad)))
    #View(v_pad_frame)
    v_1sec_frame<-rbind(v_pad_frame,v_1sec_frame )
    v_pad_frame$time=v_pad_frame$time + v_mean_interval +max(v_1sec_frame$time)
    v_1sec_frame<-rbind(v_1sec_frame, v_pad_frame)
    #plot(v_1sec_frame, type='l')
    #print("Min padded")
    #print(min(v_1sec_frame$time))
    #print("Max padded")
    #print(max(v_1sec_frame$time))
    padded_normalized_wave_for_periodgram <<-v_1sec_frame
    
    
  },
  
  createPeriodgram =function()
  {
    v_1sec_frame<-pad_1_sec()
    v_plot<-eod_spectrum()  
  },
  #end periodgram
  #normalize time(private)
  normalize_and_center_time=function(p_frame,center_type=global_time_alignment_type)
  {
	
    p_frame$time<- p_frame$time / (max(p_frame$time) - min(p_frame$time))
	p_frame<-center_time(p_frame,center_type )
	p_frame
    
  },
   center_time=function(p_frame,center_type=global_time_alignment_type)
  {
	
    
	#find center
	max_pos = which(p_frame$amplitude ==(max(p_frame$amplitude)))
	min_pos = which(p_frame$amplitude ==(min(p_frame$amplitude)))
    if(center_type=="halfway")
	{		
		interv<-c(max_pos, min_pos)			
		#if min before max
		interv_sorted<- sort(interv)		
		time_center_value = (p_frame[interv_sorted[2],1]+p_frame[interv_sorted[1],1]) /2		
				
	}
	else if(center_type=="positive_peak")
	{		
		#print("go_positive")
		time_center_value=p_frame[max_pos,1]
	}
	else if(center_type=="negative_peak")
	{
		#print("go_negative")
		time_center_value=p_frame[min_pos,1]
	}
	#print("TIME_CENTER")
	#print(time_center_value)
	#closest array point
	real_time_center_position = which(abs(p_frame$time - time_center_value)==min(abs(p_frame$time - time_center_value)))		
	real_time_center_value = p_frame[real_time_center_position, 1]		
	#substract time baseline value
	p_frame$time <- p_frame$time - real_time_center_value
	p_frame
    
  },
  inversePhase = function()
  {
    
    global_baseline_done = FALSE
    file_wave$amplitude<<-file_wave$amplitude * -1
    getPossibleBaseline(baselineType)
    return("done")
  }
  ,
  #GGPLOT
  save_plot = function(p_frame, p_title, valleys_peaks, v_frame_zero, v_frame_start_end, v_chosen_baseline_position )
  {
    tmp_plot<-ggplot(p_frame,aes(x = time, y = amplitude)) + geom_line() + ggtitle(p_title)+
      geom_point(shape=8, data= p_frame[valleys_peaks,] , fill="black", color="black", size=3) + #display peaks
      
      geom_point(shape=16,   data= v_frame_zero ,  color="red", size=3) + #x=0 (halfway between peaks)
      geom_point(shape=1,   data= v_frame_start_end ,  color="red", size=3) + #display start and stop
      geom_text(data= v_frame_start_end, label=v_frame_start_end$labels,  hjust=0,vjust=0)+
      geom_vline(aes(xintercept=p_frame$time[chosen_baseline_position]),
                 color="red", linetype="dashed", size=1)
    
    tmp_plot
  }
  #file
  ,
  savePlots = function(folder)
  {
    
    
    name_main_plot=paste0(folder, "\\",base_filename ,"_main_plot", ".png")
    name_derivate_plot=paste0(folder,"\\", base_filename,"_derivate_plot", ".png")
    name_periodgram_plot=paste0(folder,"\\", base_filename,"_periodgram_plot", ".png")
    
    ggsave(name_main_plot,getMainPlot())
    ggsave(name_derivate_plot,getDerivatePlot())
    ggsave(name_periodgram_plot,getPeriodgramPlot())
  }
  
  
)

#cluster object

EodCluster <-setRefClass("EodClusters",
                         fields = list(
                           sourceFiles="vector",
                           eodObjects="ANY",
                           baselineType="character",
                           identifiers="ANY",
                           merged_index="ANY",
                           original_files="ANY", 
						   cluster_files="ANY",					
						   time_alignment="character",
						   averaged_wave="ANY"
                         )
)

atts_clusters <- attributes(EodCluster$fields())$names
EodCluster$accessors(atts_clusters)
EodCluster$methods()

EodCluster$methods(
  initialize=
    function(sourceFiles="N/A", baselineType=global_baseline_type, true_filenames=NULL, cluster_files=NULL, encoding = global_encoding,time_alignment=global_time_alignment_type, ...)
    {
      callSuper(...)
	  time_alignment<<-time_alignment
      original_files<<-c()
	  source_files_to_eod<<-c()
	  original_files_to_eod<<-c()
	  uploaded_files_to_eod<<-c()
      objs<-list()
      baselineType<<-baselineType
      #print("ORIGINAL_NAMES")
	  #print(true_filenames)
	   #print(typeof(true_filenames))
	  
      if(!is.null(cluster_files))
      {
		cluster_files<<-cluster_files
		if(!is.null(true_filenames))
        {
            original_files<<-c()
			for(tmp in true_filenames)
		    {
			  original_files<<-c(original_files, tmp)
		    }
        }
		
		eodObjects<<-list()
		i<-1
		for(file in cluster_files)
        {
		  if(is.null(true_filenames))
          {
            original_files<<-c(original_files,file)
			readClusterFile(file)
          }
		  else
		  {
		   #print("ORIGINAL_NAME_LOOP")
		   #print(true_filenames[i])
			readClusterFile(file, original_name=true_filenames[i] )
		  }
          
		  i <- i + 1
		}
        #print("NEW_CLUSTER")
        #print(cluster_files)
		#print("ORIGINAL_FILE")
		#print(original_files)
		#print("original_files_to_eod")
		#print(original_files_to_eod)
		
        
      }
      else
      {
        sourceFiles<<-sourceFiles
        
       
        identifiers<<-data.frame(id=c(), count=c())
        i <- 1
        for(file in sourceFiles)
        {
          if(is.null(true_filenames))
          {
            original_files<<-c(true_filenames,file)
          }
          else
          {
            original_files<<-c(true_filenames,true_filenames[i])
          }
          
          eod <- Eod$new(specimenTag="init")
          eod$readFile(file)
          eod$getPossibleBaseline(baselineType)
          
          objs[[i]]<-eod
          if(eod$getSpecimenIdentifier() %in% identifiers$id )
          {
            identifiers$count[which(identifiers$id==eod$getSpecimenIdentifier())]<<-identifiers$count[which(identifiers$id==eod$getSpecimenIdentifier())]+1
          }
          else
          {
            line<-data.frame(id=c(eod$getSpecimenIdentifier()), count=c(1))
            identifiers<<-rbind(identifiers,line)
          }
          i <- i + 1
        }
		#print("DUMP_OBJS")
	    #print(objs)
        eodObjects <<- objs
      }
	  
      size <<- length(eodObjects)
	  #print("CHECK")
	  ##print(eodObjects)
      .self
    },
  getEODS=function(index)
  {
    eodObjects[[index]]
  },
  getSpecimenList = function()
  {
    identifiers
  },
  getSpecimenData=function(specimen_id)
  {
    tmp<-unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
    eodObjects[tmp==TRUE]
  },
  saveAllPlots = function(folder, specimen_id="")
  {
    if(specimen_id=="")
    {
      v_tmp<-eodObjects
    }
    else
    {
      tmp <- unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
      v_tmp <- eodObjects[tmp==TRUE]
    }
    for(eod in v_tmp)
    {
      eod$savePlots(folder)
    }
  },
  
  superimposePlots=function(specimen_id="")
  {
    if(specimen_id=="")
    {
      file_name=""
      v_tmp<-eodObjects
    }
    else
    {
      file_name=specimen_id
      tmp <- unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
      v_tmp <- eodObjects[tmp==TRUE]
    }
    df=NULL
    i=1
    cols=c("time")
    for(eod in v_tmp)
    {
	  #print(i)
      if(i==1)
      {
        df<-data.frame(eod$getNormalizedWave())
      }
      else
      {
        df<-merge(df, eod$getNormalizedWave(), by="time", all=TRUE )
      } 
      cols<-c(cols, eod$getBaseFilename())
      i <- i+1
    }
    #print(cols)
    tmp_plot<-ggplot(df, aes(x=time, y=amplitude))
    
    for(eod in v_tmp)
    {
      tmp_frame<-eod$getNormalizedWave() 
      tmp_frame$filename=eod$getBaseFilename()
      colnames(tmp_frame)<-c("time", "amplitude", "filename")
      tmp_plot<- tmp_plot + geom_line(data = tmp_frame, aes(x=time, y=amplitude, color=filename) ) 
    }
    tmp_plot
  },
  superimposeAndSavePlots=function(folder, name_file="merged_eod_plot", specimen_id="")
  {
    tmp_plot<-superimposePlots()
    name_merged_plot=paste0(folder, "\\",file_name, name_file, ".png")
    ggsave(name_merged_plot,tmp_plot)
  },
  getSize = function()
  {
    length(eodObjects)
  },
  
  getAllEods=function()
  {
    eodObjects
  },
  getSourceFiles=function()
  {
	source_files_to_eod
  },
  getSourceFile=function(index)
  {
	source_files_to_eod[index]
  },
   getOriginalFiles=function()
  {  
    original_files_to_eod
  },
   getOriginalFile=function(index)
  {  
    original_files_to_eod[index]
  },
   getUploadedFiles=function()
  {  
    uploaded_files_to_eod
  },
   getUploadedFile=function(index)
  {  
    uploaded_files_to_eod[index]
  },
  getFilenameForMergedData=function()
  {
    merged_index<<-list()
    l_files<-c()
    i=1
    invisible(lapply(eodObjects, 
          function(eod)
          {
            metadata<-eod$getMetadata()
            tagno = paste(metadata$value[1])
            project_name = paste(metadata$value[nrow(metadata)])
            tagno=tolower(str_replace(tagno, "[^[:alnum:]]",""))
            project_name=tolower(str_replace(project_name, "[^[:alnum:]]",""))
            new_file<-paste0("merge_mscope_", project_name, "_", tagno, ".csv")
            l_files<<-c(l_files,new_file)
            if(new_file %in% merged_index)
            {
              merged_index[[new_file]]<<-c(i)
            }
            else
            {
              merged_index[[new_file]]<<-c(merged_index[[new_file]],i)
            }
            i<<-i+1
          }
      ))
    ##print("merged=")
    ##print(merged_index)
    #print("ORIGINAL")
    #print(original_files)
    unique(l_files)
  }
  ,
  createMergedFile=function(new_file, target_folder)
  {
    #print("NEW_PARAM")
    #print(new_file)
    list_f<-merged_index[[new_file]]
    #print(list_f)
    fn=paste0(target_folder, "/",new_file )
    write("EOD_CLUSTER",fn, append = TRUE)
    i<-1
    invisible(lapply(list_f,
                    function(x)
                    {
                      #print(x)
                      original_file<-original_files[x]
                      #print(original_file)
                      
                      tmp_eod=eodObjects[[x]]
                      write(c("BEGIN",as.character(i), paste0("original_file","\t",original_file ),"="),fn, append = TRUE)
                      write.table(tmp_eod$getMetadata(),fn, sep="\t", append = TRUE, row.names=FALSE, quote = FALSE, dec = "." )
                      write("WAVE",fn, append = TRUE)
                      write.table(tmp_eod$getWave(),fn, sep="\t", append = TRUE, row.names=FALSE , quote = FALSE, dec = ".")
                      write("END",fn, append = TRUE)
                      i<<-i+1
                    }
                    ))
  },
  updateCluster=function( target_folder, cluster_file)
  {
	#print("file_to_save")
	#print(file_array)
	#print("target_folder")
	#print(target_folder)
	#print("cluster_file")
	#print(cluster_file)
	i<-1
	#print(uploaded_files_to_eod)
	tmp_eods<-c()
	target_file<-NULL
	source_file<-NULL
	for(file in uploaded_files_to_eod)
	{
	    
		if(file==cluster_file)
		{
			#print("to_update")
			#print(i)
			tmp_eods<-c(tmp_eods, eodObjects[[i]])
			#print("target_file")
			#print(original_files_to_eod[i])
			target_file<-original_files_to_eod[i]
			
		}
		i<-i+1
	}
	fn<-paste0(target_folder, "/",target_file)
	file.copy(fn, paste0(fn,".bck"), overwrite = TRUE )
	write("EOD_CLUSTER",fn, append = FALSE)
	i<-1
    invisible(lapply(tmp_eods,
                    function(x)
                    {
                      
                      source_file<<-source_files_to_eod[i]
                      tmp_eod=x
					  
                      write(c("BEGIN",as.character(i), paste0("original_file","\t",source_file ),"="),fn, append = TRUE)
                      write.table(tmp_eod$getMetadata(),fn, sep="\t", append = TRUE, row.names=FALSE, quote = FALSE, dec = "." )
                      write("WAVE",fn, append = TRUE)
                      write.table(tmp_eod$getWave(),fn, sep="\t", append = TRUE, row.names=FALSE , quote = FALSE, dec = ".")
                      write("END",fn, append = TRUE)
                      i<<-i+1
                    }
                    ))
  },
  readClusterFile=function(cluster_file, original_name=NULL,  encoding=global_encoding)
  {
     #print("file_to_open")
	 #print(cluster_file)
	 #print("original_name")
	 #print(original_name)
	  con = file(cluster_file, "r")
	  new_file<-FALSE
	  begin_metadata<-NULL
	  end_metadata<-NULL
	  i<-1
      i_file<-0
	  name_current_file<-c()
	  index_current_file<-c()
	  end_current_file<-c()
      nb_file<-NULL
      name_file<-NULL
	  eod<-NULL
	  offset_metadata<-4
      offset_wave<-17	  
	  while ( TRUE ) 
	  {
		line = readLines(con, n = 1)
		if ( length(line) == 0 ) {
		  break
		}
		else
		{
			
			if(new_file)
			{
			
				if(i_file==0)
				{
					nb_file<-line
				}
				else if(i_file==1)
				{
					metafile<-strsplit(line, '\t') [[1]]
					#print(metafile)
					if(length(metafile)==2)
					{
						if(metafile[1]=="original_file")
						{
							name_file<-metafile[2]
							#print("detect source file")
							#print(name_file)
							name_current_file<-c(name_current_file,name_file)
							
						}
						else
						{
							name_current_file<-c(name_current_file,"")
						}
					}
				}
				i_file<-i_file+1
			}
			if(line=="BEGIN")
			{
				new_file<-TRUE
				index_current_file<-c(index_current_file,i)
			}
			else if(line=="END")
			{
				end_current_file<-c(end_current_file,i)
				new_file<-FALSE
				i_file<-0
			}
			i<-i+1
		}
		##print(line)
	  }
	  close(con)
	  
	  decimal_sep<-"."
	  base_len<-length(eodObjects)
	  for(i in 1:length(index_current_file))
	  {
	    #print("i cluster")
		#print(i)
		pos_begin<-index_current_file[i]
		pos_end<-end_current_file[i]
		name_current<-name_current_file[i]
		
		#metadata
		file_metadata <- read.csv(cluster_file, 
                             header=FALSE,
                             skip=pos_begin+offset_metadata,							 
                             nrows=12, 
                             sep="\t", 
                             colClasses=c("character","character"),
                             fileEncoding = encoding)
		colnames(file_metadata) <- c("param", "value")
		
		eod <- Eod$new(
				  specimenTag=file_metadata$value[1],
				  provisionalId=file_metadata$value[2],
				  collectionEvent=file_metadata$value[3],
				  recordingSoftware=file_metadata$value[4],
				  recordingHardware=file_metadata$value[5],
				  recordist=file_metadata$value[6],
				  bitDepth=as.numeric(file_metadata$value[7]),
				  samplingRate = as.numeric(file_metadata$value[8]),
				  recordingDateTime = file_metadata$value[9],
				  recordingTemperature = as.numeric(file_metadata$value[10]),
				  recordingConductivity =  as.numeric(file_metadata$value[11]),
				  projectName =   file_metadata$value[12],
				)
		eod$setMetadata(file_metadata)
		eod$setFile(name_current)
		#wave
		#print(pos_begin+offset_wave+1)
		
		#print("DELIM")
		#print(decimal_sep)
		file_wave<-read.csv(
				  cluster_file,
				  sep='\t',
				  skip=pos_begin+offset_wave,
				  nrows=pos_end-(pos_begin+offset_wave+2),				  
				  header=TRUE,
				  dec=decimal_sep,
				  colClasses=c("numeric","numeric", "NULL")
				)
			
				if(ncol(file_wave)>2)
				{
				  file_wave<-file_wave[,1:2]
				}
		colnames(file_wave) <- c("time", "amplitude")
		eod$setWave(file_wave)
		eodObjects[[base_len + i]]<<-eod
		if(!is.null(original_name))
		{
			original_files_to_eod<<-c(original_files_to_eod,original_name)
		}
		uploaded_files_to_eod<<-c(uploaded_files_to_eod,cluster_file)
	  }
	  source_files_to_eod<<-name_current_file
	  #print("DONE")
	  TRUE 
  },
  removeEodAtIndex=function(i)
  {
	#print("------------------------------------------------")
    #print("remove")
	#print(i)
	eodObjects<<-eodObjects[-i]
	uploaded_files_to_eod<<-uploaded_files_to_eod[-i]
	source_files_to_eod<<-source_files_to_eod[-i]
  },
  setTimeAlignment=function(type)
  {
	time_alignment<<-type
	invisible(lapply(eodObjects, 
          function(eod)
          {
			eod$setTimeAlignment(time_alignment)
		  }
		  ))
  }
  ,
  getTimeAlignment=function()
  {
	time_alignment
  },
  find_closest_amplitude=function(time, df, control_interval)
  {
		returned<-NULL
		closest<-which.min(abs(time-df$time))
		found_interval<-abs(df$time[closest]-time)
		#print("test")
		#print("control interval")
		#print(control_interval)
		#print("test_interval")
		#print(found_interval)
		
		if(found_interval<=control_interval)
        {		
		    #print("keep")
			returned<-df$amplitude[closest]
		}
		returned
  },
  generateAveragedNormalizedSignal=function()
  {
	#take first Eod
	v_time<-c()
	v_ampl<-c()
	if(length(eodObjects)>=1)
	{
		ref_time=eodObjects[[1]]
		apply(ref_time$getNormalizedWave(),1, 
				function(x)
				{
				    tmp_ampl<-0
					tmp_ampl<-x[2]
					vector_ampl<-c(tmp_ampl)
					if(length(eodObjects)>1)
					{
						for(i in 2:length(eodObjects))
						{

							cmp_time=eodObjects[[i]]$getNormalizedWave()
							control_interval<-eodObjects[[i]]$getBiggestNormalizedTimeInterval()
							add_ampl<-find_closest_amplitude(x[1],cmp_time, control_interval)
							if(!is.null(add_ampl))
							{
								vector_ampl<-c(vector_ampl,add_ampl)
							}							
							
						}
						
						avg_ampl=sum(vector_ampl)/length(vector_ampl)						
						v_time<<-c(v_time,min(x[1]))
						v_ampl<<-c(v_ampl,avg_ampl)
					}
					
					
				}
			)
	}
	averaged_wave<<-data.frame(v_time, v_ampl)
	colnames(averaged_wave)<<-c("time", "amplitude")
	#View(averaged_wave)
  },
  averaged_to_mormyroscope_file=function(file_name)
  {
	if(!is.null(averaged_wave)&&length(eodObjects)>=1)
	{
		meta=eodObjects[[1]]$getMetadata()
		v_field=c("SpecimenTag","ProvisionallD","CollectionEvent","RecordingSoftware","RecordingHardware",
		"Recordist","BitDepth","SamplingRate","RecordingDateTime","RecordingTemperature","RecordingConductivity","ProjectName")
		v_value=c(paste0(meta$value[1],"averaged"),meta$value[1],"Averaged file, metadata on field information not significant. Compare with source files. Date is date of averaging, not coming from field work",meta$value[4],meta$value[5],
		"Non relevant (merged and averaged)",meta$value[7],meta$value[8], strftime(as.POSIXlt(Sys.time(), "UTC") , "%Y-%m-%dT%H:%M:%S%z"),"Non relevant (merged and averaged)","Non relevant (merged and averaged)","Non relevant (merged and averaged)")
		merged_metadata<-data.frame(v_field, v_value)
		colnames(merged_metadata)<-c("time", "amplitude")

		write.table(merged_metadata,file_name, sep="=", append = FALSE, row.names=FALSE,   col.names = FALSE,quote = FALSE, dec = "." )
		write("Time [s]\tAmplitude"	,file_name, append = TRUE)
		write.table(averaged_wave,file_name, sep="\t", append = TRUE, row.names=FALSE, col.names = FALSE, quote = FALSE, dec = "." )
	}
  }
  
)

