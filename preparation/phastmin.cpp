

#include <iostream>
#include <queue>
#include <vector>
#include <fstream>
#include <limits>
#include <functional>
#include <chrono>
#include <thread>
#include <Rcpp.h>




using namespace std;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

Rcpp::NumericVector Phast3_min(std::vector<int> dep,std::vector<int> arr,std::vector<int> &gfrom,std::vector<int> &gto,std::vector<double> &gw,int NbNodes, bool reversed){
  
  int size;
  if (!reversed){
    size=dep.size();
  } else{
    size=arr.size();
  }
  
 // Rcpp::NumericVector result2(arr.size());
  
  Rcpp::NumericVector result(size, std::numeric_limits<double>::max());
  
  struct comp{
    
    bool operator()(const std::pair<int, double> &a, const std::pair<int, double> &b){
      return a.second > b.second;
    }
  };
  
  //Graphs
  
  int NbEdges=gfrom.size();
  
  std::vector<std::vector<std::pair<int, double> > > G(NbNodes);   
  std::vector<std::vector<std::pair<int, double> > > Gr(NbNodes);
  
  int count=0;
  for (unsigned int i = 0; i < NbEdges; ++i) {
    
    if (gfrom[i] > gto[i])  G[gfrom[i]].push_back(std::make_pair(gto[i], gw[i]));
    if (gfrom[i] < gto[i]) Gr[gto[i]].push_back(std::make_pair(gfrom[i], gw[i]));
    count+=1;
    
  }
  for (int i=0; i < Gr.size(); i++){
    std::sort(Gr[i].begin(),Gr[i].end());
    
  }
  
  Rcpp::IntegerVector NodeGr(count);
  Rcpp::NumericVector WGr(count);
  Rcpp::IntegerVector IndGr(NbNodes+1);
  count=0;
  for (int i=0; i < Gr.size();i++){
    IndGr[i]=count;
    
    for (int j=0; j < Gr[i].size();j++){
      NodeGr[count]=Gr[i][j].first;
      WGr[count]=Gr[i][j].second;
      count+=1;
    }
  }
  IndGr[NbNodes]=count;
  std::vector<std::vector<std::pair<int, double> > > ().swap(Gr);
  
  //Forward
  
  std::vector<double> Distances(NbNodes, std::numeric_limits<double>::max()); 
  
  
  auto t1 = std::chrono::high_resolution_clock::now();
  
  
  
  for (unsigned int k=0; k!=dep.size();k++){
    if (k % 256 == 0){
      Rcpp::checkUserInterrupt ();
      std::cout << k << std::endl;
    }
    
    
    int StartNode=dep[k];
    
    Distances[StartNode] = 0.0;  
    
    
    std::priority_queue<std::pair<int, double>, std::vector<std::pair<int, double> >, comp > Q;
    Q.push(std::make_pair(StartNode, 0.0)); 
    
    
    while (true) {  
      
      if (Q.empty()){
        break;
      }  
      
      
      
      if (!Q.empty()){
        
        int v=Q.top().first;
        double w=Q.top().second;
        Q.pop();
        
        
        
        
        
        if (w <= Distances[v]) {
          for (int i=0; i< G[v].size(); i++){
            int v2 = G[v][i].first;                                                     
            double w2 = G[v][i].second;
            
            
            if (Distances[v] + w2 < Distances[v2]) {                               
              Distances[v2] = Distances[v] + w2;                                   
              Q.push(std::make_pair(v2, Distances[v2]));
              
              
            }
          }
        }
        
      }
      
      
    }
    
    
    
    //Backward
    
    for (int i=0; i < (IndGr.size()-1); i++){
      
      for (int j=IndGr[i]; j < IndGr[i+1]; j++){
        
        
        if (Distances[NodeGr[j]]+WGr[j] < Distances[i]) Distances[i] = Distances[NodeGr[j]]+WGr[j];
        
        
        
      }
      
      
    }
    
    ///
    ///  for (int i=0;i!=arr.size();i++){
    ///    if (Distances[arr[i]]==std::numeric_limits<double>::max()){
    ///      result2[i] = Rcpp::NumericVector::get_na();
    ///    }
    ///   else {
    ///     result2[i]= Distances[arr[i]];
    ///  }
      
      
      /// }
    
    
//minimum and index
    if (!reversed){
      
      
      double dist= std::numeric_limits<double>::max();
      int index= -1;
      
      for (int m=0; m < arr.size(); m++){
        if (Distances[arr[m]]==Rcpp::NumericVector::get_na()){
          continue;
        }
        if (Distances[arr[m]] < dist){
          dist = Distances[arr[m]];
          
        }
      }
      
      result[k] = dist;
      
      
    } else {
      
      
      
      for (int m=0; m < arr.size(); m++){
        
        
        if (Distances[arr[m]] < result[m]){
          result[m] = Distances[arr[m]];
        }
      }
    }

    
    std::fill(Distances.begin(),Distances.end(),std::numeric_limits<double>::max());
    //std::fill(result2.begin(),result2.end(),std::numeric_limits<double>::max());
    
  }
  
  
  
  
  return result;
  
  
}