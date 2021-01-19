# Redes Neurais

# 



# Neurônio Artificial

Para:

Tentar entender o funcionamento do neurônio no processamento de informação e;

Tentar utilizar esse modelo para processamento de informação computacional\.

O neurofisiologista McCulloch e o matemático Walter Pitts criaram um modelo matemático/computacional\.

Esse neurônio permitia múltiplas entradas de valores binários \(simulando pulsos elétricos\) e uma saída \(resultado do processamento da informação\)\.

<img src="img/RedesNeurais20204.gif" width=500px />

Cada bit de entrada Xi é multiplicado por um peso Wi \(força de influência do neurônio de origem\) e o produto interno desses dois vetores entram na Somma do neurônio \(X\.W\)\.

<img src="img/RedesNeurais20205.gif" width=500px />

<img src="img/RedesNeurais20206.gif" width=500px />

Esse neurônio é capaz de realizar algumas funções booleanas:

E

OU

NÃO

# One Layer to Rule them all?

Ela funciona bem para as portas lógicas E\, OU e NÃO\.

Ela funciona bem para as portas lógicas E\, OU e NÃO\.

Ela funciona bem para as portas lógicas E\, OU e NÃO\.

Mas essa mesma rede não pode aprender o ou exclusivo\.

# ...and in the darkness

# Redes Neurais de Múltiplas Camadas

Para ser possível mapear uma saída não\-linear precisamos de múltiplas camadas de neurônios\, formando uma rede neural da mesma forma que em nosso cérebro\.

# Regressão Logística

<span style="color:#002060">Mas para isso devemos alterar a função para:</span>

<span style="color:#002060">Y = sinal\(W\.X\)</span>

<span style="color:#002060">com a função sinal retornando \-1 se W\.X for negativo e \+1 caso contrário\.</span>

<span style="color:#002060">Mas a função sinal\, assim como a função valor absoluto\, não é diferenciável em todos os pontos\.</span>

<img src="img/RedesNeurais20207.png" width=299px />

<span style="color:#002060">A forma dela é muito similar a função sinal\, e ainda tem a vantagem de nos retornar um valor entre 0 e 1\.</span>

<img src="img/RedesNeurais20208.gif" width=500px />

<span style="color:#002060">Dessa forma Y ∈ \{0\,1\} e o valor resultante da função logística pode ser interpretado como a probabilidade de pertencer a classe 1\.</span>

<span style="color:#002060">Se f\(W\.X\) representa a probabilidade de X pertencer a classe 1\, temos que considerar a probabilidade dele pertencer a classe 0\, ou seja\, 1 – f\(W\.X\)\.</span>

<span style="color:#002060">Com isso devemos alterar algumas coisinhas no nosso modelo de regressão\.</span>

<span style="color:#002060">A primeira alteração é a função de erro\.</span>

<span style="color:#002060">O erro deve ser referente ao valor de Y\.</span>

# Redes Neurais

<span style="color:#002060">Vamos verificar seu funcionamento em um modelo mais simples:</span>

__f\(W\.x\) = sign\(W\.x\)__

<span style="color:#002060">Nesse caso temos que aprender os valores de W que melhor aproxima cada exemplo de entrada x para o respectivo y\!</span>

__f\(W\.x\) = sign\(W\.x\)__

<span style="color:#002060">Isso é a regressão linear\! \(ou quase\)</span>

__f\(W\.x\) = sign\(W\.x\)__

<span style="color:#002060">Não podemos esquecer de x0\, que tem sempre o valor 1\, aqui chamado de bias\.</span>

__f\(W\.x\) = sign\(W\.x\)__

<span style="color:#002060">A função também pode ser alterada para a logística:</span>

__f\(W\.x\) = 1/\(1 \+ e^\(\-WX\)\)__

<span style="color:#002060">E utilizamos o algoritmo de gradiente descendente para encontrar os valores de W\.</span>

__f\(W\.x\) = 1/\(1 \+ e^\(\-WX\)\)__

<span style="color:#002060">Note que aqui estamos interessados que o valor de y seja o mesmo da classe \(0 ou 1\)\.</span>

__f\(W\.x\) = 1/\(1 \+ e^\(\-WX\)\)__

<span style="color:#002060">E se tivermos mais de um neurônio na camada de saída?</span>

# One Layer to Rule them all?

<span style="color:#002060">Mas essa generalização funciona quando temos apenas a camada de entrada e de saída\.</span>

<span style="color:#002060">E assim como a regressão linear\, esse tipo de rede neural pode apenas aprender o que é linearmente separável\.</span>

<span style="color:#002060">Ela funciona bem para as portas lógicas E\, OU e NÃO\.</span>

<span style="color:#002060">Ela funciona bem para as portas lógicas E\, OU e NÃO\.</span>

# Redes Neurais

<span style="color:#002060">Mas essa mesma rede não pode aprender o ou exclusivo\.</span>

# ...and in the darkness

# Redes Neurais de Múltiplas Camadas

<span style="color:#002060">Para ser possível mapear uma saída não\-linear precisamos de múltiplas camadas de neurônios\.</span>

<span style="color:#002060">A ideia é que\, em cada camada intermediária\, com uma função de ativação não\-linear\, faça uma combinação não linear dos atributos de entrada\.</span>

<span style="color:#002060">Esse tipo de rede é conhecida como Rede Neural de Múltiplas Camadas ou MLP\.</span>

<span style="color:#002060">Obs\.: apesar da ilustração podemos ter diversas camadas intermediárias\.</span>

# MLP

<span style="color:#002060">A</span>  <span style="color:#002060">combinação das variáveis originais projet</span>  <span style="color:#002060">a</span>  <span style="color:#002060">m o plano original em outro que os exemplos sejam separáveis linearmente\.</span>

<span style="color:#002060">Isso pode ser feito também na regressão linear\, mas manualmente\.</span>

<span style="color:#002060">Nesse exemplo vamos introduzir uma nova variável x3 = x1\.x2 e projetar em um plano tridimensional</span>

<span style="color:#002060">Melhorou\! Mas ainda devemos combinar outras variáveis para obter o desejado\.</span>

# Redes Neurais de Múltiplas Camadas

<span style="color:#002060">Vamos agora determinar como treinar uma MLP\.</span>

<span style="color:#002060">w</span>  <span style="color:#002060">ijk</span>  <span style="color:#002060">=</span>  <span style="color:#002060">peso da aresta que liga o neurônio i da camada k ao neurônio j da camada k\+1\.</span>

<span style="color:#002060">Zik é igual ao produto interno do vetor de pesos que incide no neurônio i da camada k\.</span>

__Z\(1\,2\) = W\(i\,1\,1\)\.X__

__Z\(1\,3\) = W\(i\,1\,2\)\.Z\(i\,2\)__

__Z\(2\,2\) = W\(i\,2\,1\)\.X__

<span style="color:#002060">e Yik é o resultado da função de ativação do neurônio i na camada k\.</span>

__y\(1\,2\) = f\(Z\(1\,2\)\)__

__y\(1\,3\) = f\(Z\(1\,3\)\)__

__y\(2\,2\) = f\(Z\(2\,2\)\)__

<span style="color:#002060">e Yik é o resultado da função de ativação do neurônio i na camada k\.</span>

<span style="color:#002060">Agora repetimos o processo para a outra camada:</span>

<span style="color:#002060">Esse procedimento é conhecido como retro\-propagação \(back\-propagation\) pois ele emite o erro gerado na saída de volta até a entrada\.</span>

<span style="color:#002060">Vamos ilustrar o algoritmo para uma MLP de 2 camadas:</span>

<span style="color:#002060">Entrada:</span>

<span style="color:#002060">ni \- neurônios da camada de entrada</span>

<span style="color:#002060">nh \- neurônios da camada escondida</span>

<span style="color:#002060">no \- neurônios da camada de saída</span>

<span style="color:#002060">treino \- matriz com amostras de treino</span>

Inicialize:

double \[ni\]\[nh\] wi = pesos dos neurônios da primeira camada

double \[nh\]\[no\] wh = pesos dos neurônios da segunda camada

função avalia\( x\, wi\, wh \):

Para i de 0 até nh:

fh\[i\] = 0

Para j de 0 até ni:

fh\[i\] \+= wi\[j\]\[i\]\*x\[j\]

fh\[j\] = f\(fh\[j\]\)

<\-\-

Para i de 0 até no:

fo\[i\] = 0

Para j de 0 até nh:

fo\[i\] \+= wh\[j\]\[i\]\*fh\[j\]

fo\[j\] = f\(fo\[j\]\)

retorna fo\, fh

função retropropaga\( y\, fo\, fh\, wh \)

Para i de 0 até no:

deltao\[i\] = \(fo\[i\]\-y\[i\]\)\*df\(fo\[i\]\)

Para i de 0 até nh:

deltah\[i\] = 0

Para j de 0 até no:

deltah\[i\] \+= deltao\[j\]\*wh\[i\]\[j\]

deltah\[i\] \*= df\(fh\[i\]\)

retorna deltah\, deltao

função f\( x \)

retorna 1/\(1 \+ exp\(\-x\)\)

função df\(x\)

retorna x\*\(1\-x\)

função aprende\( x\, wi\, wh\, deltah\, deltao\, fh \)

Para i = 0 até nh:

Para j = 0 até no:

wh\[i\]\[j\] \-= alfa\*deltao\[j\]\*fh\[i\]

Para i = 0 até ni:

Para j = 0 até nh:

wi\[i\]\[j\] \-= alfa\*deltah\[j\]\*x\[i\]

retorna wi\, wh

função mlp\( X\,Y \)

Para it = 0 até N:

Para cada x\,y de X\,Y:

fo\, fh = avalia\( x\, wi\, wh \)

deltah\, deltao = retropropaga\(\.\.\.\)

wi\, wh = aprende\( … \)

retorna wi\, wh

# Aprendizado Hebbiano

Em 1949\, Hebb escreveu um livro intitulado “Organization of Behavior”\.

"Quando um axônio de uma célula A está próxima de excitar uma célula B e repetidamente ou persistentemente toma parte em ativá\-la\, algum processo crescente ou mudança metabólica se apossa de uma ou ambas as células de forma que a eficiência de A\, assim como a de uma das células B excitadas\, são aumentadas"\.

Ou seja\, dado que o neurônio A ativa o neurônio B frequentemente\, a sinapse \(peso na rede artificial\) é aumentada para estimular a ativação\.

Essa teoria está relacionada ao processo de memória associativa\.

Com a memória associativa podemos resgatar memórias com apenas parte de uma informação\.

<img src="img/RedesNeurais20209.jpg" width=500px />

<img src="img/RedesNeurais202010.png" width=500px />

# Rede Hopfield

Esse efeito é simulado artificialmente através da rede de hopfield\, ou rede recorrente\, onde a saída dos neurônios alimentam a entrada\.

<img src="img/RedesNeurais202011.gif" width=324px />

<img src="img/RedesNeurais202012.jpg" width=400px />

<img src="img/RedesNeurais202013.gif" width=500px />

# Aprendizagem Profunda

Teoria da dinâmica de aprendizado no cérebro\.

Camadas de redes neurais que aprendem hierarquicamente apenas parte da informação\.

Processo de auto\-organização\, resultado final é o aprendizado completo\.

Ocorre durante o período natal e em partes do pós\-natal\.

Essa teoria é uma explicação para a inteligência humana em comparação com outros animais\, inclusive primatas\.

O período em que o aprendizado por camadas ocorre é um pouco maior do que em outras espécies\, permitindo uma maior capacidade na extração e processamento de informação proveniente de estímulos externos\.

Porém isso nos torna mais dependentes de nossos pais por um maior período de tempo\.

<img src="img/RedesNeurais202014.png" width=500px />

<img src="img/RedesNeurais202015.png" width=500px />

<img src="img/RedesNeurais202016.png" width=500px />

