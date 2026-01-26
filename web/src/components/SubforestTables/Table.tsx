import "./Table.scss";
import {
  type ColumnDef,
  flexRender,
  getCoreRowModel,
  useReactTable,
} from "@tanstack/react-table";
import type { Node } from "services/HaskellApiService";

export function Table(props: {
  name: string;
  columns: ColumnDef<Node>[];
  nodes: Node[];
}) {
  const table = useReactTable({
    data: props.nodes,
    columns: props.columns,
    columnResizeMode: "onChange",
    columnResizeDirection: "ltr",
    getCoreRowModel: getCoreRowModel(),
  });

  const style = {
    fontWeight: 600,
    fontSize: "1em",
  };

  if (props.nodes.length === 0)
    return (
      <small>
        <pre style={style}>{props.name}: NOTHING</pre>
      </small>
    );

  return (
    <small style={style}>
      <pre>{props.name}</pre>
      <div style={{ direction: table.options.columnResizeDirection }}>
        <div className="overflow-x-auto">
          <table
            {...{
              style: {
                width: table.getCenterTotalSize(),
              },
            }}
          >
            <thead>
              {table.getHeaderGroups().map((headerGroup) => (
                <tr key={headerGroup.id}>
                  {headerGroup.headers.map((header) => (
                    <th
                      key={header.id}
                      {...{
                        colSpan: header.colSpan,
                        style: {
                          width: header.getSize(),
                        },
                      }}
                    >
                      {header.isPlaceholder
                        ? null
                        : flexRender(
                            header.column.columnDef.header,
                            header.getContext(),
                          )}
                      <div
                        {...{
                          onDoubleClick: () => header.column.resetSize(),
                          onMouseDown: header.getResizeHandler(),
                          onTouchStart: header.getResizeHandler(),
                          className: `resizer ${table.options.columnResizeDirection}
                        ${header.column.getIsResizing() ? "isResizing" : ""}`,
                          style: {
                            transform: header.column.getIsResizing()
                              ? `translateX(
                              ${
                                (table.options.columnResizeDirection === "rtl"
                                  ? -1
                                  : 1) *
                                (table.getState().columnSizingInfo
                                  .deltaOffset ?? 0)
                              }px)`
                              : "",
                          },
                        }}
                      />
                    </th>
                  ))}
                </tr>
              ))}
            </thead>
            <tbody>
              {table.getRowModel().rows.map((row) => (
                <tr key={row.id}>
                  {row.getVisibleCells().map((cell) => (
                    <td
                      key={cell.id}
                      {...{
                        style: {
                          width: cell.column.getSize(),
                        },
                      }}
                    >
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext(),
                      )}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
      <br />
    </small>
  );
}
