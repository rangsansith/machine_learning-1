T = readtable('winequality-combine.csv');
%T=table2array(T);
sample = randsample(1:6497,5000);
testsample=setdiff(1:6497,sample);
T1=T{sample,:};
T2 = T{testsample,:};
T1=T1';
T2=T2';
x1=T1(1:12,:);
t1=T1(13,:);
x2=T2(1:12,:);
t2=T2(13,:);


size(x1)
size(t1)

setdemorandstream(391418381)
%[x11,PS] = mapminmax(x)


net = patternnet(10);
view(net)

[net,tr] = train(net,x11,t);
nntraintool

t_pred=myNN(x2);
[C,order] = confusionmat(round(t_pred),t2)
accuracy=(C(1,1)+C(2,2)+C(3,3)+C(4,4)+C(5,5)+C(6,6)+C(7,7))/sum(sum(C))
