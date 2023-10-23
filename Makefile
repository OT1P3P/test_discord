##
## EPITECH PROJECT, 2023
## Glados
## File description:
## Makefile
##

NAME = glados

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/Glados-exe ./$(NAME)

clean:
	stack clean

fclean:	
	rm -f $(NAME)

re:	fclean all
