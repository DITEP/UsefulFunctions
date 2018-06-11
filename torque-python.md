# Torque with Python

L'IGR a un cluster de calcul a disposition pour faire tourner des tâches qui requièrent beaucoup de mémoire et de capacité de calcul.
Ce document présente rapidement comment lancer des jobs pour des scripts écrits en python.


## job classique
[Torque](http://www.adaptivecomputing.com/support/documentation-index/torque-resource-manager-documentation/) est un framework open source 
permettant de gérer l'allocation de ressources pour effectuer plusieurs tâches simultannées sur un même serveur. 
Nous utiliserons la command qsub pour lancer les tâches [documentation ici](http://docs.adaptivecomputing.com/torque/4-0-2/Content/topics/commands/qsub.htm).


Dans un premier temps, se connecter au serveur en ssh puis cloner le projet contenant vos scripts dans /data/username/workspace 

La seconde étape sera de créer un environnement virtuel, choisir la version de python adaptée au projet et un nom d'environnement (ici venv):
$ cd /data/username/workspace/project_name
$ /cm/shared/bioinfo/python/3.x.x/bin/virtualenv venv
Ceci permettra ensuite d'installer les libraires nécessaire à votre projet
$ source virtual_environment_name/bin/activate
(venv)$ pip install -r requirements requirements.txt #sinon installer à la main

Maintenant, il va falloir créer un fichier .sh contenant les paramètres que vous donnerez à Torque ainsi que les programmes à lancer.
Ce fichier se présente typiquement commme suit:




## job interactif
Parfois, certaines fonctions nécessitent d'avoir accès à un terminal (pour taper des mot de passe par exemple, c'est possible avec
le mode interactif de qsub


Pour cela, il faut se connecter sur un noeud de calcul de la manière suivante
username@login:~$ qsub -I -l walltime=10:00:00,mem=8g,nodes=1:ppn=8  #on peut rajouter les mêmes paramètres que dans le fichier .sh
qsub: waiting for job job_id to start

job_id ready
username@fatnode:~$ cd /data/username/workspace/project_name
username@fatnode:../project_name$ source venv/bin/activate
(venv)username@fatnode:../project_name$ python main.py  arg1 arg2 ...
Vous pourrez ensuite rentrer des paramètres demandés par votre main en ligne de commande

Conseil pour le mode interactif: certaines tâches peuvent durer longtemps, si vous vous deconnectez, cela aura pour effet de tuer les jobs en
cours. L'idéal est donc de se connecter sur le cluster depuis un terminal émulé par [tmux](https://github.com/tmux/tmux), 
de cette manière vous pourrez laisser tourner le programme sans avoir de terminal attaché au serveur.










