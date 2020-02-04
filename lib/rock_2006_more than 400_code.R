rock_2006.list=NULL
for(i in 1:nrow(rock_2006)){
  sentences=syuzhet::get_sentences(rock_2006$lyrics[i])
  if(length(sentences)>0){
    emotions=matrix(emotion(sentences)$emotion, 
                    nrow=length(sentences), 
                    byrow=T)
    colnames(emotions)=emotion(sentences[1])$emotion_type
    emotions=data.frame(emotions)
    emotions=select(emotions,
                   anticipation,
                   joy, 
                   surprise, 
                   trust, 
                   anger, 
                   disgust, 
                   fear, 
                   sadness)
    word.count=f.word_count(sentences)
# colnames(emotions)=paste0("emo.", colnames(emotions))
# in case the word counts are zeros?
# emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
    rock_2006.list=rbind(rock_2006.list, 
                        cbind(rock_2006[i,-ncol(rock_2006)],
                              sentences=as.character(sentences), 
                              word.count,
                              emotions,
                              sent.id=1:length(sentences)
                              )
    )
  }
}
names(rock_2006.list)

#save(rock_2006.list,file="C:/Users/Saier/Desktop/courses/5243/rock_2006_list.RData")