import os
import json
import csv

#crawl data from rg


#json to csv
path = 'E:/rg_answer/'
l = os.listdir('E:/rg_answer')
with open("data/table/question.csv","a+",encoding="UTF-8",newline="") as question_file:
    f_q = csv.writer(question_file)
    f_q.writerow(['filename','text','name','time'])
    for i in l:
        filename = i.split('.')[0].replace('-','_')
        print(filename)
        with open(path+i, 'r', encoding='utf8') as fp:
            json_data = json.load(fp)
        post_text = json_data['post_text']
        post_time = json_data['post_time']
        post_names = json_data['post_name']
        for post_name in post_names:
            f_q.writerow([filename,post_text,post_name,post_time])

with open("data/table/answer.csv","a+",encoding="UTF-8",newline="") as answer_file:
    f_a = csv.writer(answer_file)
    f_a.writerow(['filename','text','name'])
    num = 0
    for i in l:
        filename = i.split('.')[0].replace('-','_')
        #print(filename)
        with open(path+i, 'r', encoding='utf8') as fp:
            json_data = json.load(fp)
        answer_text = json_data['answer_text']
        answer_names = json_data['answer_name']

        #如果text比names长，有用户注销，text赋空值
        if len(answer_text) > len(answer_names):
            for j in range(0, len(answer_names)):
                f_a.writerow([filename,' ',answer_names[j]])
        # 如果names比answer长,保留后部和answer等长部分
        elif len(answer_names) > len(answer_text):
            answer_names_ = answer_names[-len(answer_text):]
            try:
                for j in range(0,len(answer_text)):
                    f_a.writerow([filename,answer_text[j],answer_names_[j]])
            except:
                print(filename)
                print(len(answer_names))
                print(len(answer_text))
                print(len(answer_names_))
        else:
            for j in range(0,len(answer_text)):
                f_a.writerow([filename,answer_text[j],answer_names[j]])

with open("data/table/follow_recommend.csv","a+",encoding="UTF-8",newline="") as fr_file:
    f_fr = csv.writer(fr_file)
    f_fr.writerow(['filename','follow_num','follow_name','recommend_num','recommend_name'])
    for i in l:
        filename = i.split('.')[0].replace('-','_')
        print(filename)
        with open(path+i, 'r', encoding='utf8') as fp:
            json_data = json.load(fp)
        try:
            follow_num = json_data['followers_num'][0]
            recommend_num = json_data['rec_num'][0]
            follow_name = json_data['followers']['name']
            recommend_name = json_data['recommendations']['name']
            f_fr.writerow([filename,follow_num,follow_name,recommend_num,recommend_name])
        except:
            f_fr.writerow([filename, '', '', '', ''])

path_href = 'F:/href/'
l_h = os.listdir('F:/href')
with open("data/table/topic_to_file.csv","a+",encoding="UTF-8",newline="") as file:
    f = csv.writer(file)
    f.writerow(['topic','filename'])
    for i in l_h:
        filename = i.split('.')[0]
        if 'inf' == filename[-3:]:
            continue
        else:
            print(filename)
            with open(path_href + i, 'r', encoding='utf8') as fp:
                data = fp.readlines()
                for j in data:
                    j = j.replace('post/','').replace('-','_')
                    f.writerow([filename, j])

import re
with open("data/table/topic.csv","a+",encoding="UTF-8",newline="") as file:
    f = csv.writer(file)
    f.writerow(['topic','topics','type','question_num','publication_num'])
    for i in l_h:
        filename = i.split('.')[0]
        if 'inf' != filename[-3:]:
            continue
        else:
            topic = filename.replace('_inf','')
            #print(topic)
            with open(path_href + i, 'r', encoding='utf8') as fp:
                data = fp.readlines()
                if '\n' in data:
                    data.remove('\n')
                row_num = len(data)
                if row_num  >= 6:
                    topics = data[0].split(';')
                    type = data[1]
                    question_num = data[-2]
                    pn = data[-1]
                    num = re.findall("\d+", pn)
                    publication_num = "".join(num)
                    f.writerow([topic, topics, type, question_num, publication_num])
                elif row_num >= 2:
                    topics = data[0].split(';')
                    type = data[1]
                    f.writerow([topic, topics, type, None, 0])
                else:
                    print(topic)
                    print(row_num)
                    print('row_num different')

#get other data
