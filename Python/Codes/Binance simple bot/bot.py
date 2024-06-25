import websocket 
import json
import numpy as np
import datetime

SOCKET = "wss://stream.binance.com:9443/ws/btcusdt@kline_1m"



PRECO_COMPRA = 65000 #em dolares
RENTABILIDADE = 0.05 #5%
PRECO_VENDA = PRECO_COMPRA + (PRECO_COMPRA * RENTABILIDADE)
VALOR_COMPRA = 20 #em dolares

TRADE_SYMBOL = 'BTCUSD'




in_position = False
closes = []
close_compra = 0

def on_open(ws):
    print("opened connection")

def on_close(ws):
    print("closed connection")

def timestamp_converter(timestamp):
    return datetime.datetime.fromtimestamp(timestamp / 1e3)
def on_message(ws, message):
    print("Mensagem recebida")
    global closes
    json_message = json.loads(message) 
    #print(json_message)
    candle = json_message['k']
    fechamento_candle = candle['x']
    print(fechamento_candle == True)
    if (fechamento_candle == True):
        close = candle['c']
        print("________")
        print(f"data: {timestamp_converter(candle['t'])}")
        print(f"fechamento: {close}")
        closes.append(float(close))
        if(close <= PRECO_COMPRA and in_position == False):
            in_position = True
            print(f"comprando ${VALOR_COMPRA},00 ")
            close_compra = close
        elif(VALOR_COMPRA < PRECO_VENDA )
            
        print("________")
    else:
        print("candle is not closed")
        print('')

        
            


    


ws = websocket.WebSocketApp(SOCKET, on_open=on_open, on_close=on_close, on_message=on_message, )
ws.run_forever()