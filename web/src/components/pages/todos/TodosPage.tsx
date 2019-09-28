import * as React from "react";
import { Spinner } from "react-bootstrap";
import Todo from "../../../services/entities/Todo";
import todo_backend from "../../../services/TodoApiService";
import BaseContainerPage from "../BaseContainerPage";
import { TodoRepresentation } from "./TodoRepresentation";
import TodosWriter from "./TodosWriter";

export interface ITodosPageProps {}

export interface ITodosPageState {
  todos: Map<number, Todo> | null;
}

export default class TodosPage extends React.Component<ITodosPageProps, ITodosPageState> {
  constructor(props: ITodosPageProps) {
    super(props);

    this.state = {
      todos: null,
    };

    this.addTodo = this.addTodo.bind(this);
    this.toggleTodo = this.toggleTodo.bind(this);
  }

  private updateTodos() {
    todo_backend.getTodos().then(todos => {
      todos.sort((a: Todo, b: Todo) => b.id - a.id);

      let todos_map: Map<number, Todo> = new Map();
      for (let t of todos) {
        todos_map.set(t.id, t);
      }

      this.setState({ todos: todos_map });
    });
  }

  componentDidMount() {
    this.updateTodos();
  }

  addTodo(todo: Todo) {
    if (this.state.todos !== null) {
      this.setState({ todos: new Map([[todo.id, todo], ...(this.state.todos as any)]) });
    }
  }

  toggleTodo(todo: Todo) {
    if (this.state.todos !== null) {
      let newTodos = new Map(this.state.todos);
      todo.completed = !todo.completed;
      newTodos.set(todo.id, todo);
      this.setState({ todos: newTodos });

      todo_backend.updateTodo(todo);
    }
  }

  deleteTodo(id: number) {
    if (this.state.todos !== null) {
      let newTodos = new Map(this.state.todos);
      newTodos.delete(id);
      this.setState({ todos: newTodos });

      todo_backend.deleteTodo(id);
    }
  }

  renderTodos() {
    if (this.state.todos === null) {
      return (
        <div className="d-flex align-items-center flex-column">
          <Spinner animation="border" className="font-weight-light d-block mb-4" variant="primary" />
          <h6>Loading existing todos...</h6>
        </div>
      );
    } else {
      return (
        <>
          {[...this.state.todos.values()].map(todo => (
            <TodoRepresentation
              key={todo.id}
              todo={todo}
              toggleHandler={e => this.toggleTodo(todo)}
              deleteHandler={e => this.deleteTodo(todo.id)}
            />
          ))}
        </>
      );
    }
  }

  render() {
    return (
      <BaseContainerPage title="Todos REST API test" lead="Test interaction with backend by managing your todo list.">
        <TodosWriter newTodoHandler={this.addTodo} />
        {this.renderTodos()}
      </BaseContainerPage>
    );
  }
}
