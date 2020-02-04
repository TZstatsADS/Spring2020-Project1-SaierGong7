sentence.list=NULL
for(i in 1:nrow(new_lyric)){
 sentences=syuzhet::get_sentences(new_lyric$lyrics[i])
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
 #colnames(emotions)=paste0("emo.", colnames(emotions))
 #in case the word counts are zeros?
 #emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
    sentence.list=rbind(sentence.list, 
                        cbind(new_lyric[i,-ncol(new_lyric)],
                              sentences=as.character(sentences), 
                              word.count,
                              emotions,
                              sent.id=1:length(sentences)
                              )
    )
  }
}
names(sentence.list)

#delete some genres that contains much less songs
sentence.list<-sentence.list%>%
  filter(genre!='Folk') %>%
  filter(genre!='Indie') %>%
  filter(genre!='R&B') %>%
  filter(!is.na(word.count))

#hai sheng xia de genre 1."Hip-Hop"    2."Pop"  3."Metal"   4."Rock"  5."Country"    6."Jazz"   7."Not Available"  8."Electronic"   