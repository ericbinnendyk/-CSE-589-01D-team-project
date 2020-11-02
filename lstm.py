import os
from matplotlib.pyplot import axis
import requests
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler

def download(url, dir, fname):
    response = requests.get(url)
    with open(os.path.join(dir, fname), 'wb') as f:
        f.write(response.content)

def data_processing():
    new_cases = pd.read_csv('data/new_cases.csv')
    #new_deaths = pd.read_csv('data/new_deaths.csv')
    world_nc= new_cases[['date', 'World']].to_numpy()[30:, 1]
    print(world_nc.shape)
    #exit()
    #plt.figure()
    #lw=2
    #plt.plot(world_nc, lw = lw)
    #plt.title('world new_cases every data')
    #plt.ylabel('new cases every day')
    #plt.grid(True)
    #plt.autoscale(axis='x', tight=True)
    #fn = 'figures/world_nc.png'
    #plt.savefig(fn)
    return world_nc
    
def trn_tst_split(data, tst_size):
    trn_data = data[: -tst_size]
    tst_data = data[-tst_size: ]
    print(trn_data.shape)
    print(tst_data.shape)
    #scaler = MinMaxScaler(feature_range = (-1,1))
    #trn_normalized = scaler.fit_transform(trn_data.reshape(-1,1))
    #print(trn_normalized.shape)
    #print(trn_normalized.tail())
    #trn_normalized = torch.FloatTensor(trn_normalized).view(-1)
    return trn_data, tst_data
    
def create_inout_sequences(input_data, train_window):
    inout_seq =[]
    L = len(input_data)
    for i in range(L-train_window):
        trn_seq = input_data[i: i+train_window]
        trn_lbl = input_data[i+train_window:i+train_window+1]
        inout_seq.append((trn_seq, trn_lbl))
    print(inout_seq[:5])
    print(type(inout_seq))
    print(len(inout_seq))
    return inout_seq
    

class LSTM(nn.Module):
    def __init__(self, input_size=1, hidden_layer_size = 100, output_size=1):
        super().__init__()
        self.hidden_layer_size = hidden_layer_size
        self.lstm = nn.LSTM(input_size, hidden_layer_size)
        self.linear = nn.Linear(hidden_layer_size, output_size)
        self.hidden_cell = (torch.zeros(1,1,self.hidden_layer_size), torch.zeros(1,1,self.hidden_layer_size))
        
    def forward(self, input_seq):
        lstm_out, self.hidden_cell = self.lstm(input_seq.view(len(input_seq),1,-1), self.hidden_cell)
        predictions = self.linear(lstm_out.view(len(input_seq), -1))
        return predictions[-1]    
    

if __name__ == "__main__":
    
    #_download("https://covid.ourworldindata.org/data/ecdc/new_cases.csv", 'data/','new_cases.csv')
    #_download("https://covid.ourworldindata.org/data/ecdc/new_deaths.csv", 'data/','new_deaths.csv')
    test_size=15
    world_nc = data_processing()
    
    trn_data, tst_data = trn_tst_split(world_nc, test_size)
    print(trn_data[1:100])
    scaler = MinMaxScaler(feature_range = (-1,1))
    trn_normalized = scaler.fit_transform(trn_data.reshape(-1,1))
    #trn_data2 = scaler.inverse_transform(trn_normalized)
    #print(trn_data2[1:100])
    #exit()
    #print(trn_normalized.tail())
    trn_normalized = torch.FloatTensor(trn_normalized).view(-1)
    train_window = 30
    train_inout_seq = create_inout_sequences(trn_normalized, train_window)
    #test_inout_seq = create_inout_sequences(tst_normalized, train_window)
    lstm_model = LSTM()
    loss_function = nn.MSELoss()
    optimizer = torch.optim.Adam(lstm_model.parameters(), lr=0.001)
    print(lstm_model)
    
    epochs = 100
    for i in range(epochs):
        for seq, lables in train_inout_seq:
            optimizer.zero_grad()
            lstm_model.hidden_cell =(torch.zeros(1,1, lstm_model.hidden_layer_size), torch.zeros(1,1,lstm_model.hidden_layer_size))
            y_pred = lstm_model(seq)
            single_loss = loss_function(y_pred, lables)
            single_loss.backward()
            optimizer.step()
        if i%10==1:
            print(f'epoch: {i:3} loss: {single_loss.item(): 10.8f}')
            
    print(f'epoch: {i:3} loss: {single_loss.item(): 10.10f}')
    
    
    test_inputs = trn_normalized[-train_window:].tolist()
    print(test_inputs)
    
    lstm_model.eval()
    for i in range(test_size):
      seq = torch.FloatTensor(test_inputs[-train_window:])
      with torch.no_grad():
          lstm_model.hidden = (torch.zeros(1,1, lstm_model.hidden_layer_size), torch.zeros(1,1,lstm_model.hidden_layer_size))
          test_inputs.append(lstm_model(seq).item())
          print(test_inputs)
    #scaler = MinMaxScaler(feature_range = (-1,1))      
    actual_predictions = scaler.inverse_transform(np.array(test_inputs[train_window:]).reshape(-1,1))
    print(actual_predictions)
    print(tst_data)  
    x=np.arange(0,test_size,1)
    print(x)
    
    #plt.title('new_cases everyday')
    #plt.ylabel('new cases')
    #plt.grid(True)
    #plt.autoscale(axis='x', tight=True)
    #plt.plot(world_nc, lw=2)
    #plt.plot(x,actual_predictions)
    #fn ='figures/all_pred.png'
    #plt.savefig(fn)
    print(tst_data)
    plt.title('new_cases predict for seven days')
    plt.ylabel('new cases')
    plt.grid(True)
    plt.autoscale(axis='x', tight=True)
    plt.plot(tst_data, lw=2, color='red')
    plt.plot(x,actual_predictions, color='blue')
    fn =f'figures/test_pred_{test_size}.png'
    plt.savefig(fn)
            
            
            
            
            
            
