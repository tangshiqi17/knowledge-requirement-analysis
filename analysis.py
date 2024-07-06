import pandas as pd
import networkx as nx
import re
import ast
import numpy as np
import csv
import json


#get name list
answer = pd.read_csv('data/table/answer.csv')
question = pd.read_csv('data/table/question.csv')
fr = pd.read_csv('data/table/follow_recommend.csv')

def re_list(i):
    try:
        return ast.literal_eval(i)
    except:
        return []

fr['follow_name'] = fr['follow_name'].apply(re_list)
fr['recommend_name'] = fr['recommend_name'].apply(re_list)
follow = []
rec = []
for i in fr['follow_name']:
    follow.extend(i)
for i in fr['recommend_name']:
    rec.extend(i)
names = list(set(list(answer['name']))|set((list(question['name'])))|set(follow)|set(rec))
print(len(names))

#分离firstname和lastname
firstnames = []
lastnames = []

for name in names:
    first_name = name.split(' ')[0].lower()
    last_name = name.split(' ')[-1].lower()
    firstnames.append(first_name)
    lastnames.append(last_name)

#get region
with open('data/new/name_country.json') as f1:
    data1 = json.load(f1)
with open('data/new/name_country1.json') as f2:
    data2 = json.load(f2)
data2.update(data1)
df = pd.DataFrame.from_dict(data2, orient='index',columns=['country'])
df = df.reset_index().rename(columns = {'index':'name'})

iso_country = pd.read_csv("data/new/country_iso.txt",delimiter="\t")
iso_country = iso_country[['ISO','ISO3','Country']]

#分离国家简写和非简写
def abbr(i):
    if len(i) == 2 and str(i).isupper():
        return 1
    else:
        return 0
df['abbr'] = df['country'].apply(abbr)

data = pd.DataFrame(columns=['name','ISO','ISO3','Country'])

for i in df.groupby('abbr'):
    if i[0] == 1:
        i[1].columns = ['name','ISO','abbr']
        print(len(i[1]))
        d = pd.merge(i[1],iso_country,on='ISO')
        print(len(d))
        data = pd.concat([data,d])
    elif i[0] == 0:
        i[1].columns = ['name','Country','abbr']
        print(len(i[1]))
        d = pd.merge(i[1],iso_country,on='Country')
        print(len(d))   #简写和非简写都有没对应上的
        data = pd.concat([data,d])
print(data)

data = data[['name','ISO3','ISO']]
data.columns = ['name','Country Code','ISO']
country_region = pd.read_csv('data/new/country_region_for_world_map.csv')
print(list(set(list(data['Country Code']))-set(list(country_region['Country Code']))))
print(len(data))
df = pd.merge(data,country_region,on='Country Code')
print(len(df))  #those regions not in the world bank are not included. 'GGY', 'CXR', 'AIA', 'MYT', 'TWN', 'JEY', 'GUF'

#gender identification using genderize.io
#new_gender.csv

#gender identification using sex machine
import sexmachine.detector as gender
firstnames = []

for name in df['name']:
    first_name = name.split(' ')[0]
    firstnames.append(first_name)

d = gender.Detector(unknown_value=u"unknown")
gender = []
for i in range(0,len(df['name'])):
    if d.get_gender(firstnames[i]) != 'unknown':
        gender.append(d.get_gender(firstnames[i]))
    else:
        l = str(df['name'].loc[i]).split(' ')
        if len(l) >= 3:
            if d.get_gender(l[1]) != 'unknown':
                gender.append(d.get_gender(l[1]))
            else:
                if len(l) >= 4:
                    gender.append(d.get_gender(l[2]))
                else:
                    gender.append('unknown')
        else:
            gender.append('unknown')

g = pd.DataFrame({'gender':gender})
df = pd.concat([df,g],axis=1)
#print(df)
s = df.groupby('gender')
for i in s:
    print(i[0])
    print(len(i[1]))
df.to_csv('data/gender.csv',index=False,quoting=1)

# assign topics to fields
df = pd.read_csv('data/label_tang.csv')
d = df[['topic_first_category_t','dis_t']]
d.columns = ['first','dis']
d['dis'] = d['dis'].apply(str)

topic = pd.read_csv('data/table/topic.csv')
question = pd.read_csv('data/table/question.csv')
n = pd.read_csv('data/name_region_gender.csv')
topic_to_file = pd.read_csv('data/table/topic_to_file.csv')
answer = pd.read_csv('data/table/answer.csv')

def drop_n(i):
    return str(i).strip('\n')

topic_to_file['filename'] = topic_to_file['filename'].apply(drop_n)
def re_l(i):
    return ast.literal_eval(i)[0].strip('\t').strip('\n')

topic['first'] = topic['topics'].apply(re_l)

df = pd.merge(question,topic_to_file,on='filename')
df = pd.merge(df,topic,on='topic')
df = pd.merge(df,n,on='name')
df = pd.merge(df,d,on='first')

df.to_csv('data/dis/question/all.csv',index=False)
s = df.groupby('dis')
for i in s:
    if i[0] == '1.0':
        i[1].to_csv('data/dis/question/Arts & Humanities.csv',index=False)
    if i[0] == '2.0':
        i[1].to_csv('data/dis/question/Life Sciences & Biomedicine.csv', index=False)
    if i[0] == '3.0':
        i[1].to_csv('data/dis/question/Physical Sciences.csv', index=False)
    if i[0] == '4.0':
        i[1].to_csv('data/dis/question/Social Sciences.csv', index=False)
    if i[0] == '5.0':
        i[1].to_csv('data/dis/question/Technology.csv', index=False)
    if i[0] == '6.0':
        i[1].to_csv('data/dis/question/other.csv', index=False)

df_a = pd.merge(answer,topic_to_file,on='filename')
df_a = pd.merge(df_a,topic,on='topic')
df_a = pd.merge(df_a,n,on='name')
df_a = pd.merge(df_a,d,on='first')

df_a.to_csv('data/dis/answer/all.csv',index=False)
s_a = df_a.groupby('dis')
for i in s_a:
    if i[0] == '1.0':
        i[1].to_csv('data/dis/answer/Arts & Humanities.csv',index=False)
    if i[0] == '2.0':
        i[1].to_csv('data/dis/answer/Life Sciences & Biomedicine.csv', index=False)
    if i[0] == '3.0':
        i[1].to_csv('data/dis/answer/Physical Sciences.csv', index=False)
    if i[0] == '4.0':
        i[1].to_csv('data/dis/answer/Social Sciences.csv', index=False)
    if i[0] == '5.0':
        i[1].to_csv('data/dis/answer/Technology.csv', index=False)
    if i[0] == '6.0':
        i[1].to_csv('data/dis/answer/other.csv', index=False)

#calculate RCA
all_question = pd.read_csv('data/dis/question/all.csv')
all_question = all_question[['filename', 'text', 'name', 'topic','question_num','publication_num','dis']]
df_q = pd.merge(all_question,df,on='name')

#RCA(r,i) = (该地区女性提问数/该地区提问总数)/(所有地区女性提问总数/所有地区研究者提问总数)
f = open('data/new/fig1_map_region.csv','w',encoding='utf-8')
csv_writer = csv.writer(f)
csv_writer.writerow(["RCA","gender","Region","dis"])
for d in df_q.groupby('dis'):
    if d[0] == 6.0:
        continue
    all_q = d[1]
    all_r = d[1]['name'].drop_duplicates()
    all_w_q = d[1][d[1]['gender']=='female']
    all_w = all_w_q['name'].drop_duplicates()
    all_m_q = d[1][d[1]['gender']=='male']
    all_m = all_w_q['name'].drop_duplicates()
    all_q_num = len(all_q)/len(all_r)
    all_w_q_num = len(all_w_q)/len(all_w) #总体女性平均提问数
    all_m_q_num = len(all_m_q)/len(all_m)  #总体平均提问数
    for i in d[1].groupby('Region'):
        try:
            r_q = i[1]
            r_r = i[1]['name'].drop_duplicates()
            r_w_q = i[1][i[1]['gender']=='female']
            r_w = r_w_q['name'].drop_duplicates()
            r_m_q = i[1][i[1]['gender']=='male']
            r_m = r_m_q['name'].drop_duplicates()
            r_q_num = len(r_q)/len(r_r) #地区平均提问数
            r_w_q_num = len(r_w_q)/len(r_w) #女性平均提问数
            r_m_q_num = len(r_m_q)/len(r_m)
            rca_w = (r_w_q_num/r_q_num)/(all_w_q_num/all_q_num)
            rca_m = (r_m_q_num/r_q_num)/(all_m_q_num/all_q_num)
            csv_writer.writerow([rca_w,'female',i[0],d[0]])
            csv_writer.writerow([rca_m,'male',i[0],d[0]])
        except:
            print(i[0])

r = pd.read_csv('data/new/fig1_map_region.csv')
c = pd.read_csv('data/new/country_region_for_world_map.csv')
r = pd.merge(r,c,on='Region')
r.to_csv('data/new/fig1_map_country.csv',index=False)

#normalize
df = pd.read_csv('data/fig1_map_region.csv')
df_c = pd.read_csv('data/fig1_map_country.csv')
df_c = df_c[['gender','Region','dis','Country Name']]
df['minmax'] = ''
max_min_scaler = lambda x: (x - np.min(x)) / (np.max(x) - np.min(x))
#男女分别归一化
for i in df.groupby('gender'):
    if i[0] == 'female':
        female = i[1]
    else:
        male = i[1]
female['minmax'] = female[['RCA']].apply(max_min_scaler)
male['minmax'] = male[['RCA']].apply(max_min_scaler)
df = pd.concat([female,male])
df = pd.merge(df_c,df,on=['Region','gender','dis'])
print(df)
df.to_csv('data/fig1_minmax.csv',index=False)

#this part is for visualization, making country searched by affiliation and it's name in the world map same
#df = pd.read_csv('data/fig1_minmax.csv')
df = pd.read_csv('data/extended data/fig1_gender_check.csv')
def change_name(i):
    if i == "Cote d'Ivoire":
        return 'Ivory Coast'
    elif i == 'St. Lucia':
        return 'Saint Lucia'
    elif i == 'British Virgin Islands':
        return 'Virgin Islands'
    elif i == 'Yemen, Rep.':
        return 'Yemen'
    elif i == 'St. Martin (French part)':
        return 'Saint Martin'
    elif i == 'West Bank and Gaza':
        return 'Palestine'
    elif i == 'St. Kitts and Nevis':  #KNA
        return 'Saint Kitts'
    elif i == 'Brunei Darussalam':
        return 'Brunei'
    elif i == 'Sint Maarten (Dutch part)':
        return 'Sint Maarten'
    elif i == 'Cabo Verde':
        return 'Cape Verde'
    elif i == 'Eswatini':
        return 'Swaziland'
    elif i == 'Congo, Rep.':
        return 'Republic of Congo'
    elif i == 'Hong Kong SAR, China':
        return 'China'
    elif i == 'Channel Islands':  #CHI
        return 'Jersey'
    elif i == 'St. Vincent and the Grenadines': #VCT
        return 'Saint Vincent'
    elif i == 'Micronesia, Fed. Sts.':
        return 'Micronesia'
    elif i == 'Macao SAR, China':
        return 'China'
    elif i == 'Antigua and Barbuda': #ATG
        return 'Antigua'
    return i
df['Country Name'] = df['Country Name'].apply(change_name)
#有些行需要复制并修改值
#newdf = df[(df['Country Code'] == "KNA") | (df['Country Code'] == "CHI") | (df['Country Code'] == "VCT") | (df['Country Code'] == "ATG")]
newdf = df[(df['Country Name']=='Saint Kitts')|(df['Country Name']=='Jersey')|(df['Country Name']=='Saint Vincent')|(df['Country Name']=='Antigua')]
def change_again(i):
    if i == 'Saint Kitts':
        return 'Nevis'
    elif i == 'Jersey':
        return 'Guernsey'
    elif i == 'Saint Vincent':
        return 'Grenadines'
    elif i == 'Antigua':
        return 'Barbuda'
    else:
        return i
newdf['Country Name'] = newdf['Country Name'].apply(change_again)
df = pd.concat([df,newdf])
print(df)
#df.to_csv('data/fig1_minmax_total.csv',index=False)
df.to_csv('data/extended data/fig1_gender_check_total.csv')

#female and male topic distribution
def get_topic_data(df,flag,fig):

    if fig == 3:
        df = df[df['dis']==2.0]
    df_ = df[['topic','dis']]
    df_.drop_duplicates(inplace=True)
    s = df.groupby('topic')

    topic_ = []
    times_ = []
    fem_ = []
    region_pro_ = []
    region_ = []
    mean_fr = []
    for i in s:
        topic = i[0]
        qa = len(i[1]) #总的问题数量
        d = i[1][['name','Region','gender']]
        d.drop_duplicates(keep='first',inplace=True,subset='name')  #d是研究者全体
        numbers = len(d)
        #gender:
        gender = d.groupby('gender')
        female = 0
        for j in gender:
            if j[0] == 'female':
                female = len(j[1])
        fem = female/numbers  #该topic下提问女性占提问者数量的比例
        if flag == 'question':
            fr_num = i[1]['follow_num'].sum()/qa #该topic下平均每个提问获得的follow数量
        elif flag == 'answer':
            fr_num = i[1]['recommend_num'].sum() / qa
        for k in d.groupby('Region'):  #该topic下不同region的提问者占总提问者数量的比例
            region_.append(k[0])
            region_pro = len(k[1])/numbers
            topic_.append(topic)
            times_.append(qa)
            fem_.append(fem)
            region_pro_.append(region_pro)
            if flag == 'question' or flag == 'answer':
                mean_fr.append(fr_num)
    topic_ = pd.DataFrame({'topic':topic_})
    times_ = pd.DataFrame({'times':times_})
    fem_ = pd.DataFrame({'fem':fem_})
    region_ = pd.DataFrame({'region':region_})
    region_pro_ = pd.DataFrame({'region_pro': region_pro_})
    if flag == 'question':
        fr_ = pd.DataFrame({'mean_follow':mean_fr})
    elif flag == 'answer':
        fr_ = pd.DataFrame({'mean_recommend':mean_fr})
    if flag == 'question' or flag == 'answer':
        df = pd.concat([topic_,times_,fem_,region_,region_pro_,fr_],axis=1)
    #分离出不同区域的数据

    df.dropna(inplace=True,subset=['times'])
    #将times改为比例
    sum_times = df['times'].sum()
    df['proportion'] = df['times']/sum_times
    #针对times排序，保留前250个topic
    if fig == 3:
        df_topic = df[['topic','times']].drop_duplicates()
        df_topic = df_topic.sort_values(by='times',ascending=False)  #出现次数最多的250个topic
        l_250 = list(df_topic.head(250)['topic'])
        print(list(df_topic.head(20)['topic']))
        df = df[df['topic'].isin(l_250)]

    print(df)
    df = pd.merge(df,df_,on='topic')
    df.dropna(subset=['dis'],inplace=True)
    return df

gender = pd.read_csv('data/gender.csv')
gender = gender[gender['gender']!='unknown']
all_question = pd.read_csv('data/all.csv')
all_question = all_question[['filename', 'text', 'name', 'topic','question_num','publication_num','dis']]
all_question = all_question[all_question['dis']!=6.0]
follow = pd.read_csv('data/follow_recommend.csv')
follow = follow[['filename','follow_num']]
all_question = pd.merge(all_question,follow,on='filename')
df = pd.merge(all_question,gender,on='name')
#df_all = get_topic_data(df,'question',2)
#df_all.to_csv('data/picture/fig2 question.csv',index=False)
df_all = get_topic_data(df,'question',3)
df_all.to_csv('data/picture/fig3 question.csv',index=False)

#build the question-answer network
def cen(d,df_node):
    df = pd.DataFrame(pd.Series(d), columns=['centrality'])
    df = df.reset_index().rename(columns={'index': 'name'})
    #直接dict转dataframe，通过name和gender merge
    df = pd.merge(df,df_node,on='name')
    return df

gender = pd.read_csv('data/gender.csv')
gender = gender[gender['gender']!='unknown']
all_question = pd.read_csv('data/all.csv')
all_question = all_question[['filename', 'text', 'name', 'topic','question_num','publication_num','dis']]
all_answer = pd.read_csv('data/all_answer.csv')
all_answer = all_answer[['filename', 'text', 'name', 'topic','question_num','publication_num','dis']]
df_a = pd.merge(all_answer,gender,on='name')
df_a.drop_duplicates(inplace=True)
print(len(df_a))
df_q = pd.merge(all_question,gender,on='name')
df_q.drop_duplicates(inplace=True)
print(len(df_q))
#构建整体问答网络

def net(df_q,df_a,flag=None,flag_value='total'):
    #print(df_q)
    #print(df_q['dis'])
    if flag == 'topic':
        df_q = df_q[df_q['topic'] == flag_value]  # Staining 'COVID-19'
        df_a = df_a[df_a['topic'] == flag_value]
    if flag == 'field':
        df_q = df_q[df_q['dis'] == flag_value]
        df_a = df_a[df_a['dis'] == flag_value]
    #print(df_q)
    #为空则构建整体网络
    df_q = df_q[['filename','name','Region','gender']]
    df_a = df_a[['filename','name','Region','gender']]
    df_n = pd.merge(df_a[['filename','name']],df_q[['filename','name']],on='filename')
    df_n = df_n[df_n['name_x']!=df_n['name_y']] #不管自问自答
    df_n.drop(columns='filename', inplace=True)
    #name_x answer    name_y question    name_x to name_y
    G = nx.DiGraph()
    df_node = pd.concat([df_q[['name','gender','Region']], df_a[['name','gender','Region']]])
    df_node.drop_duplicates(inplace=True)
    df_node.reset_index(inplace=True)
    for i, row in df_node.iterrows():
        G.add_node(row["name"], gender=row["gender"], Region=row["Region"])
    edges = []
    df_edge = []
    for i in df_n.groupby(['name_x','name_y']):
        edges.append(i[0]+({'time':len(i[1])},))
        df_edge.append([i[0][0],i[0][1],len(i[1])])
    df_edge = pd.DataFrame(df_edge)
    df_edge.columns = ['from','to','weight']
    G.add_weighted_edges_from(edges)

    print('number of nodes', G.number_of_nodes())
    print('number of edges', G.number_of_edges())
    #属性同配
    gender_asso = nx.attribute_assortativity_coefficient(G,"gender")
    region_asso = nx.attribute_assortativity_coefficient(G,"Region")
    #度同配
    deg_asso = nx.degree_assortativity_coefficient(G)
    clustering = nx.average_clustering(G)
    #转为df 列名network_type gender_asso region_asso deg_asso clustering
    dic = {'network_type':flag_value,'gender_asso':gender_asso,'region_asso':region_asso,'deg_asso':deg_asso,\
           'clustering':clustering}
    df_asso = pd.DataFrame([dic])

    d_in = nx.in_degree_centrality(G)
    d_out = nx.out_degree_centrality(G)

    df_in = cen(d_in,df_node)
    df_out = cen(d_out,df_node)
    df_in['inout'] = 'in-degree'
    df_out['inout'] = 'out-degree'
    df = pd.concat([df_in, df_out])
    df['network_type'] = flag_value
    return df_asso,df

#data for fig 4
#区域性别对比图
df_asso_total,df_total = net(df_q,df_a,flag_value='total')
df_asso_1,df_1 = net(df_q,df_a,flag='field',flag_value=1.0)
df_asso_2,df_2 = net(df_q,df_a,flag='field',flag_value=2.0)
df_asso_3,df_3 = net(df_q,df_a,flag='field',flag_value=3.0)
df_asso_4,df_4 = net(df_q,df_a,flag='field',flag_value=4.0)
df_asso_5,df_5 = net(df_q,df_a,flag='field',flag_value=5.0)
df_asso = pd.concat([df_asso_total,df_asso_1,df_asso_2,df_asso_3,df_asso_4,df_asso_5])
df = pd.concat([df_total,df_1,df_2,df_3,df_4,df_5])
df_asso.to_csv('data/picture/fig4A_asso.csv',index=False)
df.to_csv('data/picture/fig4A_inout.csv',index=False)

#topic对比
df_asso_covid,df_covid = net(df_q,df_a,flag='topic',flag_value='COVID-19')
df_asso_stain,df_stain = net(df_q,df_a,flag='topic',flag_value='Staining')
df_asso = pd.concat([df_asso_covid,df_asso_stain])
df = pd.concat([df_covid,df_stain])
df_asso.to_csv('data/picture/fig4B_asso.csv',index=False)
df.to_csv('data/picture/fig4B_inout.csv',index=False)

#supplementary analysis
with open('name_contribution.json') as f:
  data = json.load(f)

names = []
cons = []
for name, contribution in data.items():
    #print(name)
    l_name = name.split(' ')
    abb = ''
    for l in l_name:
        abb += l[0]+'.'
    #print(abb)
    #print(contribution)
    con_num = 0
    for i in contribution:
        con = i.split(';')
        for j in con:
            if (abb in j and 'experiment' in j) or (name in j and 'experiment' in j):
                con_num += 1
    #print(con_num)
    names.append(name)
    cons.append(con_num)

names = pd.DataFrame({'name':names})
cons = pd.DataFrame({'contri':cons})
df = pd.concat([names,cons],axis=1)

df_tech = pd.read_csv('2index_gender_tech_num.csv')
df_ = pd.merge(df,df_tech,on='name')
def zero(i):
    if i == 0:
        return 0
    else:
        return 1
df_['contri0'] = df_['contri'].apply(zero)
df_['tech0'] = df_['tech_pro'].apply(zero)

male = df_[df_['gender']=='male']
con_male = male[male['contri0']==1]
tech_male = male[male['tech0']==1]
join_male = male[(male['contri0']==1) & (male['tech0']==1)]
print(len(male))
print(len(con_male)/len(male))
print(len(tech_male)/len(male))
print(len(join_male)/len(male))
female = df_[df_['gender']=='female']
con_female = female[female['contri0']==1]
tech_female = female[female['tech0']==1]
join_female = female[(female['contri0']==1) & (female['tech0']==1)]
print(len(female))
print(len(con_female)/len(female))
print(len(tech_female)/len(female))
print(len(join_female)/len(female))
print(con_male.describe())
print(con_female.describe())
df_.to_csv('index_contri.csv',index=False)

df = pd.read_csv('index_contri.csv')
male = df[df['gender']=='male']
female = df[df['gender']=='female']
male_con = male[male['contri0']==1]
female_con = female[female['contri0']==1]
l = [['gender','contri','percent'],\
     ['male','1',len(male_con)/len(male)],\
     ['male','0',(len(male)-len(male_con))/len(male)],\
     ['female','1',len(female_con)/len(female)],\
     ['female','0',(len(female)-len(female_con))/len(female)]]
d = pd.DataFrame(l[1:],columns=l[0])
d.to_csv('实验贡献作者比例.csv',index=False)

df = pd.read_csv('index_contri.csv')
#去除contri异常值
out=[]
def iqr_outliers(df):
    #q1 = df.quantile(0.25)
    #q3 = df.quantile(0.75)
    #iqr = q3-q1
    #Upper_tail = q3 + 1.5 * iqr
    Upper_tail = df.quantile(0.99)
    for i in df:
        if i > Upper_tail:
            out.append(i)
    return out
out = iqr_outliers(df['contri'])
df = df[~df['contri'].isin(out)]
df.to_csv('index_contri_abn.csv',index=False)

with open('rg_name_subject_paper.json') as f:
  data = json.load(f)

names = []
diss = []
authors = []
ids = []
for name, value in data.items():
    print(name)
    for dis, value_ in value.items():
        print(dis)
        for author, id, in value_.items():
            print(author)
            print(len(id))
            for i in id:
                names.append(name)
                diss.append(dis)
                authors.append(author)
                ids.append(i)

names = pd.DataFrame({'name':names})
subjects = pd.DataFrame({'subject':diss})
authors = pd.DataFrame({'author':authors})
ids = pd.DataFrame({'id':ids})
df = pd.concat([names,subjects,authors,ids],axis=1)
df.to_csv('name_sub_paper.csv',index=False)

df = pd.read_csv('name_sub_paper.csv')
sub = [11,13,24,27,28,29,30,34,35,36] #area 2 5
df = df[df['subject'].isin(sub)]
df.drop(columns='subject',inplace=True)
df.drop_duplicates(inplace=True)
print(len(df))
print(len(list(set(list(df['name'])))))
#author相同 time相加
df_time_ = df.groupby(['name','author'],as_index=False)['id'].count()
df_time_ = df_time_.groupby(['name','author'],as_index=False)['id'].sum()
#计算一作/通讯的文章比例
df_first = df_time_[df_time_['author']==1]
df_first.drop(columns='author',inplace=True)
df_first.columns = ['name','first']
df_time = df_time_.groupby(['name'],as_index=False)['id'].sum()
df_time = pd.merge(df_time,df_first,on='name')
df_time['first_pro'] = df_time['first']/df_time['id']
def counter(i):
    return 1-i
df_time['not_first_pro'] = df_time['first_pro'].apply(counter)
print(df_time)
#计算平均排序指数
with open('eid_authors.json') as f:
  aun = json.load(f)
aun = dict(aun)
#print(aun)
def get_aun(id):
    try:
        return aun[str(id)]
    except:
        return ;
df['author_number'] = df['id'].apply(get_aun)
df.dropna(inplace=True)
print(df)
df_pro = pd.DataFrame({'name':[],'pro':[]})
for d in df.groupby('name'):
    #print(d[0])
    d[1]['pro'] = d[1]['author']/d[1]['author_number']
    #print(d[1])
    d_ = d[1].groupby(['name'],as_index=False)['pro'].sum()
    d_1 = d[1].groupby(['name'],as_index=False)['pro'].count()
    d_['pro'] = d_['pro']/d_1['pro']
    df_pro = pd.concat([df_pro,d_])
df_pro.reset_index(inplace=True)
df_new = pd.merge(df_time,df_pro,on='name')
df_new = df_new[['name','not_first_pro','pro']]
df_new.to_csv('2index.csv',index=False)

df = pd.read_csv('2index.csv')
df = df[df['pro']<=1]
print(len(df))
gender = pd.read_csv('D:\PycharmProjects\gender改\data\gender.csv')
gender = gender[['name','gender']]
gender = gender[gender['gender']!='unknown']
df = pd.merge(df,gender,on='name')
print(len(df))
df.to_csv('2index_gender.csv',index=False)

#topic参与
ls = pd.read_csv('LSdata.csv')
ls = ls[['name','topic','gender']]
ls = ls[ls['gender']!= 'unknown']
mesh = pd.read_csv('mesh.csv')
mesh = mesh[['topic','tech']]
mesh = mesh[mesh['tech']!=2]
df = pd.merge(ls,mesh,on='topic')
name = []
tech_pro = []
for i in df.groupby('name'):
    name.append(i[0])
    d = i[1]
    tech = len(d[d['tech']==1])
    not_tech = len(d[d['tech']==0])
    #tech_pro.append(tech/(tech+not_tech))
    tech_pro.append(tech)
name = pd.DataFrame({'name':name})
tech_pro = pd.DataFrame({'tech_pro':tech_pro})
df_tech = pd.concat([name,tech_pro],axis=1)
index2 = pd.read_csv('2index_gender.csv')
index2 = pd.merge(index2,df_tech,on='name')
print(len(index2))
index2.to_csv('2index_gender_tech_num.csv',index=False)

df = pd.read_csv('name_sub_paper.csv') #138774476篇文章 33704作者
tree = pd.read_csv('mesh_tree.csv',delimiter=';') #tree_node_description
tree = tree[['tree_node','tree_node_description']]
tree.columns = ['tree_node','descriptor']
concept = pd.read_csv('mesh_concept.csv',delimiter=';') #concept
concept = concept[['concept','descriptor_ui']]
concept.columns = ['descriptor','mesh_descriptor_ui']
tree_des = pd.merge(tree,concept,on='descriptor')
data04 = pd.read_csv('eid_mesh0-4.csv',delimiter='\t')
data59 = pd.read_csv('eid_mesh5-9.csv',delimiter='\t',names=['work_id','eid','mesh_seq','mesh_descriptor_ui',\
                                                             'mesh_qualifier_ui','is_major_topic'])
data = pd.concat([data04,data59])
data = data[['eid','mesh_descriptor_ui']]
data.columns = ['id','mesh_descriptor_ui']
data = pd.merge(data,tree_des,on='mesh_descriptor_ui')
df = pd.merge(df,data,on='id')
df.to_csv('keyword_raw.csv',index=False)

#后续处理，包括识别类别以及添加性别
gender = pd.read_csv('LSdata.csv')
gender = gender[['name','gender']]
gender.drop_duplicates(inplace=True)
df = pd.read_csv('keyword_raw.csv')
df = pd.merge(df,gender,on='name')
print(len(df))
def category(i):
    return str(i)[0]
df['category'] = df['tree_node'].apply(category)
df = df[['name','id','category','gender','author']]
df.to_csv('keyword_category.csv',index=False)

df = pd.read_csv('name_sub_paper.csv')
df = df[['name','id']]
df.drop_duplicates(inplace=True)
df.columns = ['name','eid']
#加性别
gender = pd.read_csv('LSdata.csv')
gender = gender[['name','gender']]
gender.drop_duplicates(inplace=True)
df = pd.merge(df,gender,on='name')
#加pmid
mid = pd.read_csv('eid_pmid.csv',delimiter=';')
df = pd.merge(df,mid,on='eid')
df.to_csv('pmid_transfer.csv',index=False)

df = pd.read_csv('pmid_transfer.csv')
trans = pd.read_csv('F:\pmid\icite_metadata\icite_metadata.csv')
df = pd.merge(df,trans,on='pmid')
df.to_csv('pmid_transfer_data.csv',index=False)

df = pd.read_csv('pmid_transfer_data.csv')
df = df[['name','gender','apt','human','y_coord']]
print(len(list(set(list(df['name'])))))
print(len(df))

#作者发表文章中mesh类别为technique文章所占比例
df = pd.read_csv('keyword_category.csv')
#df.drop_duplicates(inplace=True)
#df.to_csv('keyword_category.csv',index=False)
gender = df[['name','gender']].drop_duplicates()
def pro(df):
    l = [['name','tech_pro']]
    for i in df.groupby('name'):
        print(i[0])
        t = len(i[1][i[1]['category']=='E'])
        #s = len(list(set(list(i[1]['id']))))
        #l.append([i[0],t/s])
        l.append([i[0], t])
    d = pd.DataFrame(l[1:],columns=l[0])
    d = pd.merge(d,gender,on='name')
    return d
d = pro(df)
d.to_csv('tech文章数量.csv',index=False)
#一作（通讯）文章占比
df1 = df[df['author']==1]
d1 = pro(df1)
d1.to_csv('一作tech文章数量.csv',index=False)

#pmid_trnasfer_data.csv添加author位置列
kc = pd.read_csv('keyword_category.csv')
kc = kc[['name','id','author']]
kc.drop_duplicates(inplace=True)
kc.columns = ['name','eid','author']
pt = pd.read_csv('pmid_transfer_data.csv')
pt = pt[['name','eid','gender','pmid','human','animal','molecular_cellular','apt']]
pt = pd.merge(pt,kc,on=['name','eid'])
pt.to_csv('pmid_transfer_author.csv',index=False)

#compare researchers have and don't have articles
#提问总数
researcher = pd.read_csv('LSdata.csv')
researcher = researcher[['filename','name','topic','gender']]
have_paper = pd.read_csv('name_sub_paper.csv')
have_paper = have_paper[['name']].drop_duplicates()
print(len(have_paper))
no_paper = researcher[~researcher['name'].isin(list(have_paper['name']))]
have_paper = researcher[researcher['name'].isin(list(have_paper['name']))]
no_paper['article'] = 'no'
have_paper['article'] = 'have'
paper = pd.concat([no_paper,have_paper])
total = paper.groupby(['name','gender','article'],as_index=False)['filename'].count()

#提问技术问题数
#都是E类的topic是0，空值为2，其余为1
mesh = pd.read_csv('mesh.csv')
mesh = mesh[['topic','tech']]
researcher = pd.merge(researcher,mesh,on='topic')
tech_res = researcher[researcher['tech']==0]
no_paper_tech = tech_res[~tech_res['name'].isin(list(have_paper['name']))]
have_paper_tech = tech_res[tech_res['name'].isin(list(have_paper['name']))]
no_paper_tech['article'] = 'no'
have_paper_tech['article'] = 'have'
paper_tech = pd.concat([no_paper_tech,have_paper_tech])
tech = paper_tech.groupby(['name','gender','article'],as_index=False)['filename'].count()
tech.columns = ['name','gender','article','tech']
#提问技术问题的比例
total = pd.merge(total,tech,on=['name','gender','article'])
total['pro'] = total['tech']/total['filename']
print(total)
total.to_csv('提出技术问题占比.csv',index=False)
