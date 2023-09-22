function cohenSutherland(xmin , xmax, ymin , ymax , x1 , y1 , x2, y2){
    var m = ((y2 - y1)/(x2-x1));

    var Esquerda = (m * (xmin - x1) + y1);
    var Direita = (m * (xmax - x1) + y1);
    var Topo = (x1 + (1/m * (ymax - y1)));
    var Base = (x1 + (1/m * (ymin - y1)));

    console.log(m);
    console.log("O resultado a esquerda é: " + Esquerda);
    console.log("O resultado a direita é:  " + Direita);
    console.log("O resultadp da Base é:    " + Base);
    console.log("O resultado do topo é:    " + Topo);

    if (x1 >= xmin && x1 <= xmax && y1 >= ymin && y1 <= ymax &&
        x2 >= xmin && x2 <= xmax && y2 >= ymin && y2 <= ymax) {
        console.log("Os pontos estão dentro do quadrante: (" + x1 + ", " + y1 + "), (" + x2 + ", " + y2 + ")");
    } else {
        console.log("A projeção do ponto no eixo de corte da reta é: (" + Esquerda + ", " + Direita + "), (" + Base + ", " + Topo + ")");
    }
}
