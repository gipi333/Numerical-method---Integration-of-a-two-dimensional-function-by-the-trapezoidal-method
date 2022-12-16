! Matlab program
 
 
C = fscanf(fopen('graphes.txt','r'), '%f',[99,99]);
[X,Y] = meshgrid(-4.9:.1:4.9);
surf(X,Y,C)
