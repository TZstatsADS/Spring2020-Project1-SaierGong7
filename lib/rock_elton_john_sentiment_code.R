rock.elton_john.list=NULL
for(i in 1:nrow(rock.elton_john)){
  sentences=syuzhet::get_sentences(rock.elton_john$lyrics[i])
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
    rock.elton_john.list=rbind(rock.elton_john.list, 
                        cbind(rock.elton_john[i,-ncol(rock.elton_john)],
                              sentences=as.character(sentences), 
                              word.count,
                              emotions,
                              sent.id=1:length(sentences)
                              )
    )
  }
}
names(rock.elton_john.list)

#save(rock.elton_john.list,file="C:/Users/Saier/Desktop/courses/5243/rock_elton_john_list.RData")