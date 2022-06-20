# Neem vaste delta waarde
# Varieer epsilon, neem bv afwijking 0.002 tot 0.05 in steps van 0.003
# Output per zo'n combinatie eerst minimale m
# Ga over m/2, 3m/4, m, 1.5*m, 2*m? en output gemiddelde afwijking + kans

# Doe zelfde voor variabele delta (0.002 tot 0.05 in steps van 0.003?)

# output naar csv file
# Maak aparte grafiek foor vaste delta/epsilon
# Show op grafiek m waardes als eigen kleur


loopCount = 1000;
checkSamples = 1000;
minValue = -10000.0;
maxValue = 10000.0;


#Get amount of dimensions as input
# dimensions = readline(prompt="Aantal dimensies:");
# dimensions = as.numeric(dimensions);
dimensions = 2;

#Input epsilon, 0.002 - 0.05 in steps of 0.003
#e = readline(prompt="Foutmarge van hypothese:");
#e = as.numeric(e);
e = 0.01;

file.create("C:\\Users\\jaspe\\Documents\\School\\Jaar4 Semester1\\Bachelorproef_21-22\\output2.csv");

#input delta
# d = readline(prompt="Kans op foute hypothese is:");
# d = as.numeric(d);
current_d = 0.002
all_d = c(current_d);
for(i in 1:16){
	current_d = current_d + 0.003
	all_d = c(all_d, current_d);
}


df = data.frame(e=NA, d=NA, m=NA, mean=NA, chance=NA);
for(d in all_d){

	#minimum m voor goed resultaat volgens PAC
	minimum_m = 4 * log10(4/d)/e;
	#print(paste0("De minimum sample size m voor gegarandeerd aan PAC te voldoen is:", ceiling(minimum_m)));
	

	#input m
	#m = readline(prompt="sample size:");
	#m = as.numeric(m);
	
	#make different m's
	all_m = c(minimum_m/2, 3*minimum_m/4, minimum_m, 1.5*minimum_m, 2*minimum_m);
	
	for(m in all_m){

		correctness_vec = c();
		#loop over multiple attempts:
		loops = (1:loopCount);
		for(i in loops){
			#Make solution function (= rectangle)
			solution_rectangle = list();
			#get an a < b for every dimension
			for(dim in 1:dimensions){
				#Because we can't sample over R, we'll put a limit of +-10,000
				#The max for a = max-1 to prevent problem for for instance a = 9,999.99...
				a = runif(1, minValue, maxValue - 1);
				b = runif(1, a, maxValue);
				solution_rectangle[[dim]] = c(a,b);
			}
			
			#generate S having m samples. every sample is a point with a binary label (1 if in rectangle)
			S = list();
			for(sample in 1:m){
				#generate point
				point = c();
				for(dim in 1:dimensions){
					coordinate = runif(1, minValue, maxValue);
					point = c(point, coordinate);
				}
				
				#check whether point lies inside the rectangle
				inside = TRUE;
				for(dim in 1:dimensions){
					if(point[dim] < solution_rectangle[[dim]][1] || point[dim] > solution_rectangle[[dim]][2]){
						inside = FALSE;
					}
				}
				
				
				#Add point and label to S
				if(inside){
					S[[sample]] = c(point, 1);
				} else {
					S[[sample]] = c(point, 0);
				}
			}
			
			
			#Get h using ERM on samples, we take the minimal rectangle covering all positively labeled points.
			#minimal/maximal value for each dimenion when label is 1
			min = rep(maxValue,dimensions);
			max = rep(minValue,dimensions);
			for(sample in S){
				#if label is 1
				if(sample[dimensions + 1] > 0.5){
					for(dim in 1:dimensions){
						#get minimum
						if(sample[dim] < min[dim]){
							min[dim] = sample[dim]
						}
						#get maximum
						if(sample[dim] > max[dim]){
							max[dim] = sample[dim]
						}
					}
				}
			}
			
			#h is the rectangle labeling min[j] < point < max[j] positively for every dimension j.
			#we now compare this to the solution function for random samples to check accuracy.
			solution = list();
			ERMsolution = list();
			count_equals = 0.0;
			for(sample in 1:checkSamples){
				#generate point
				point = c();
				for(dim in 1:dimensions){
					coordinate = runif(1, minValue, maxValue);
					point = c(point, coordinate);
				}
				
				#check whether point lies inside the solution rectangle
				inside_solution = TRUE;
				inside_ERM = TRUE;
				for(dim in 1:dimensions){
					#check solution
					if(point[dim] < solution_rectangle[[dim]][1] || point[dim] > solution_rectangle[[dim]][2]){
						inside_solution = FALSE;
					}
					#check ERM hypothesis
					if(point[dim] < min[dim] || point[dim] > max[dim]){
						inside_ERM = FALSE;
					}
				}
				
				#Check correctness
				if(inside_solution == inside_ERM){
					count_equals = count_equals + 1.0;
				}
			}
			correctness_vec = c(correctness_vec, count_equals/checkSamples);

		}

		#We now calculate how many times our h had a bigger error than epsilon.
		score_to_beat = 1.0 - e;
		chance_count = 0.0;
		for(i in correctness_vec){
			if(i >= score_to_beat){
				chance_count = chance_count + 1.0;
			}
		}
		#print(paste0("De gemiddelde error van h over ", loopCount, " iteraties is ", 1 - mean(correctness_vec)));
		chance = chance_count/loopCount;
		#print(paste0("De kans op een verkeerd h over deze ", loopCount ," iteraties is ", 1 - chance));
		df[nrow(df) + 1,] = c(e, d, m, 1-mean(correctness_vec), 1-chance);
	}
}

write.csv(df, file="C:\\Users\\jaspe\\Documents\\School\\Jaar4 Semester1\\Bachelorproef_21-22\\output2.csv");