#!/usr/bin/env python
# coding: utf-8

# Import libraries

from transformers import AutoTokenizer, AutoModelForSequenceClassification
import numpy as np
import torch 
import math
import pandas as pd
import time

# Create model

finbert = AutoModelForSequenceClassification.from_pretrained("ProsusAI/finbert")
tokenizer = AutoTokenizer.from_pretrained("ProsusAI/finbert")

# Import dataset

df = pd.read_csv ('####',sep= ";")
#print (df)

# Create vectors

sentence = df['text']
dat = df['created_at']
id = df['id']
#print(*sentence,sep="\n")
#print(dat)

#Control code fot the lenght of the tokens
alert=0
i=0
t11 = time.time()
for ind in range(len(sentence)):
    inputs2 = tokenizer(sentence[ind] ,return_tensors="pt",padding=True)
    if len(inputs2['input_ids'][0]) >= 512: 
       alert=1
       i=ind

if alert == 1: print( "Attenzione la stringa n  " + str(i) + " con id numero " +  str(id[i]) +" produce troppi indici") 
else: print("ok")

t12 = time.time()
print(t12-t11)


# Definition functions

def softmax(x):
    """Compute softmax values for each sets of scores in x."""
    e_x = np.exp(x - np.max(x, axis=1)[:, None])
    return e_x / np.sum(e_x, axis=1)[:, None]

def ciclo(limits):
   t1=time.time()
   labels = {0:'positive', 1:'negative',2:'neutral'}
   results = pd.DataFrame(columns=['id','data','sentence',
                                  'logit_positive','logit_negative','logit_neutral', 
                                  'prediction', 'sentiment_score'])
   for idk in range(limits):
       inputs = tokenizer(sentence[idk] ,return_tensors="pt",padding=True)
       #print(idk)
       with torch.no_grad():                
           logits = softmax(np.array(finbert(**inputs)[0]))
           prediction = labels[np.argmax(logits)]
           sentiment_score= pd.Series(logits[:, 0] - logits[:, 1])
           first_result = { 'id' : id[idk],
		                    'data' : dat[idk],
                            'sentence': sentence[idk],
                            'logit_positive': logits[0][0],
                            'logit_negative': logits[0][1],
                            'logit_neutral': logits[0][2],
                            'prediction': prediction,
                            'sentiment_score': sentiment_score}
           first_result = pd.DataFrame(first_result)
           results= pd.concat([results, first_result], ignore_index=True)
   t2=time.time()
   print(t2-t1)
   return results

# Start the analysis

lim = len(sentence)
out = ciclo(lim)
print(out)

# Save the results

out.to_csv('#####', sep=';', index=False , encoding='utf-8-sig')

