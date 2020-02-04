rock.bob_dylan.list=NULL
for(i in 1:nrow(rock.bob_dylan)){
  sentences=get_sentences(rock.bob_dylan$lyrics[i])
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
    rock.bob_dylan.list=rbind(rock.bob_dylan.list, 
                        cbind(rock.bob_dylan[i,-ncol(rock.bob_dylan)],
                              sentences=as.character(sentences), 
                              word.count,
                              emotions,
                              sent.id=1:length(sentences)
                              )
    )
  }
}
names(rock.bob_dylan.list)

#save(rock.bob_dylan.list,file="C:/Users/Saier/Desktop/courses/5243/rock_bob_dylan_list.RData")